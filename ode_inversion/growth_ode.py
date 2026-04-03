"""
ODE Detection for Bacterial Growth Data
========================================
Bacterial growth models are nonlinear — the linear ODE approach finds
oscillatory solutions that are locally fitting the S-shape, not the
true model structure.

This module handles three strategies:
  1. Transform-space linear ODE  (Gompertz linearises in log-space)
  2. Nonlinear ODE detection via SINDy-style polynomial feature library
  3. Sigmoid feature detection   (K, lag time, inflection point, μ_max)

Models covered:
  - Logistic:         f' = r·f·(1 - f/K)
  - Gompertz:         f' = r·f·ln(K/f)          (linear ODE for ln f)
  - Baranyi-Roberts:  lag + exponential + stationary
  - Modified Gompertz / Richards (generalised logistic)
"""

import numpy as np
from scipy.interpolate import UnivariateSpline
from scipy.optimize import curve_fit, minimize
from scipy.ndimage import gaussian_filter1d
import sympy as sp
import warnings
warnings.filterwarnings('ignore')


# ─────────────────────────────────────────────────────────────────────────────
# 1.  Gaussian derivative helper (reused from ode_robust.py)
# ─────────────────────────────────────────────────────────────────────────────

def smooth_and_deriv(xs, ys, sigma_px=8, max_order=2, noise_level=None):
    """
    Resample to uniform grid, apply Gaussian derivative filters.
    Returns (xs_u, D) where D[:,k] = estimated k-th derivative.
    """
    n = len(xs)
    s = 0 if (noise_level is None or noise_level == 0) else n * noise_level**2 * 2
    spl = UnivariateSpline(xs, ys, k=5, s=s)
    xs_u = np.linspace(xs[0], xs[-1], max(n, 200))
    ys_u = spl(xs_u)
    dx = xs_u[1] - xs_u[0]

    edge = int(np.ceil(3 * sigma_px))
    edge = min(edge, (len(xs_u) - 20) // 2)

    cols = []
    for k in range(max_order + 1):
        d = gaussian_filter1d(ys_u, sigma=sigma_px, order=k, mode='nearest') / dx**k
        cols.append(d[edge:-edge] if edge > 0 else d)

    xs_trim = xs_u[edge:-edge] if edge > 0 else xs_u
    return xs_trim, np.column_stack(cols), ys_u


# ─────────────────────────────────────────────────────────────────────────────
# 2.  Sigmoid feature detection (model-class identification)
# ─────────────────────────────────────────────────────────────────────────────

def clean_xy(xs, ys):
    """Sort by x, remove duplicate x values (keep first), return clean arrays."""
    xs, ys = np.array(xs, dtype=float), np.array(ys, dtype=float)
    order = np.argsort(xs)
    xs, ys = xs[order], ys[order]
    # Remove duplicates (keep first occurrence)
    _, unique_idx = np.unique(xs, return_index=True)
    return xs[unique_idx], ys[unique_idx]


def detect_sigmoid_features(xs, ys):
    """
    Extract key features of a growth curve:
      - y_min, y_max (asymptotes)
      - inflection point (t_infl, y_infl) — maximum growth rate
      - max growth rate μ_max = f'(t_infl)
      - lag time λ (x-intercept of tangent at inflection)
      - symmetry: is the inflection at 50% of range? (logistic) or lower? (Gompertz)
    """
    xs, ys = clean_xy(xs, ys)
    spl = UnivariateSpline(xs, ys, k=5, s=0)
    xs_dense = np.linspace(xs[0], xs[-1], 1000)
    ys_dense = spl(xs_dense)
    dy_dense = spl(xs_dense, 1)

    y_min = float(ys_dense[0])
    y_max = float(np.max(ys_dense))
    y_range = y_max - y_min

    # Inflection point: maximum of |dy/dt|
    idx_infl = int(np.argmax(np.abs(dy_dense)))
    t_infl = float(xs_dense[idx_infl])
    y_infl = float(ys_dense[idx_infl])
    mu_max = float(dy_dense[idx_infl])

    # Lag time: x-intercept of tangent at inflection
    # y_infl + mu_max*(t - t_infl) = y_min  →  t_lag = t_infl - (y_infl - y_min)/mu_max
    if abs(mu_max) > 1e-10:
        lag = t_infl - (y_infl - y_min) / mu_max
    else:
        lag = xs[0]

    # Inflection relative position in [0,1]
    infl_rel = (y_infl - y_min) / (y_range + 1e-30)

    # Monotone? (growth curves should be monotone increasing)
    is_monotone = bool(np.all(dy_dense >= -0.01 * mu_max))

    # Saturation: does the curve flatten at the end?
    last_quarter = ys_dense[750:]
    is_saturating = float(np.std(last_quarter) / (y_range + 1e-30)) < 0.05

    return {
        'y_min':     y_min,
        'y_max':     y_max,
        'y_range':   y_range,
        't_infl':    t_infl,
        'y_infl':    y_infl,
        'mu_max':    mu_max,
        'lag':       lag,
        'infl_rel':  infl_rel,    # ~0.5 → logistic;  ~0.37 → Gompertz
        'monotone':  is_monotone,
        'saturating': is_saturating,
    }


# ─────────────────────────────────────────────────────────────────────────────
# 3.  Transform-space linear ODE (for Gompertz / log-linear models)
# ─────────────────────────────────────────────────────────────────────────────

def detect_ode_in_transform(xs, ys, transforms=None):
    """
    Apply each transform g = T(f), then check if g satisfies a simple linear ODE.
    Gompertz:  g = ln(f)  satisfies g' + c·g = const (i.e. g' + c·g - d = 0)
    Logistic:  g = ln(f/(K-f)) satisfies g' = r (constant)
    """
    from ode_robust import (detect_linear_ode_svd, detect_linear_ode_stridge,
                             round_ode_coefficients, ode_to_string,
                             build_derivative_matrix_gaussian, gcv_bandwidth)

    if transforms is None:
        # Try several transforms; provide K estimate for logistic variants
        K_est = np.max(ys) * 1.05   # slightly above max
        transforms = {
            'ln(f)':          lambda y: np.log(np.clip(y, 1e-12, None)),
            'ln(K-f)':        lambda y: np.log(np.clip(K_est - y, 1e-12, None)),
            'ln(f/(K-f))':    lambda y: np.log(np.clip(y / np.clip(K_est - y, 1e-12, None), 1e-12, None)),
            '1/f':            lambda y: 1.0 / np.clip(y, 1e-12, None),
            'sqrt(f)':        lambda y: np.sqrt(np.clip(y, 0, None)),
            'f^2':            lambda y: y**2,
        }

    results = []
    n = len(xs)
    s = 0
    spl_orig = UnivariateSpline(xs, ys, k=5, s=s)
    xs_u = np.linspace(xs[0], xs[-1], max(n, 200))

    for name, T in transforms.items():
        ys_orig_u = spl_orig(xs_u)
        try:
            ys_t = T(ys_orig_u)
        except Exception:
            continue
        if not np.all(np.isfinite(ys_t)):
            continue

        sigma_px, _ = gcv_bandwidth(ys_t, max_order=2)
        D, edge = build_derivative_matrix_gaussian(xs_u, ys_t, max_order=2,
                                                    sigma_px=sigma_px)

        # Include a constant column (for ODEs like g' + c·g = d → g' + c·g - d = 0)
        ones = np.ones((D.shape[0], 1))
        D_aug = np.hstack([D, ones])

        c_svd, res_svd, _ = detect_linear_ode_svd(D_aug)
        c_str, res_str, _ = detect_linear_ode_stridge(D_aug, threshold=0.05)

        best_c = c_svd if res_svd <= res_str else c_str
        best_res = min(res_svd, res_str)

        results.append({
            'transform': name,
            'residual':  best_res,
            'c':         best_c,
            'ode_str':   ode_to_string(best_c[:-1]) + (
                             f' + const' if abs(best_c[-1]) > 0.05 else ''),
        })

    results.sort(key=lambda r: r['residual'])
    return results


# ─────────────────────────────────────────────────────────────────────────────
# 4.  Nonlinear ODE detection (SINDy-style polynomial feature library)
# ─────────────────────────────────────────────────────────────────────────────

def build_nonlinear_feature_library(f, f_prime, poly_degree=2, include_t=True,
                                     xs=None):
    """
    Build a library of candidate right-hand-side terms for f' = Θ(f,t)·c:

      [1, f, f², f³, t, t·f, t·f², f·ln|f|, f²·ln|f|, ...]

    Returns (Theta, feature_names) where Theta @ c ≈ f_prime.
    """
    n = len(f)
    features = []
    names = []

    # Polynomial in f
    for d in range(poly_degree + 1):
        features.append(f**d)
        names.append(f'f^{d}' if d > 1 else ('1' if d == 0 else 'f'))

    # Include t (non-autonomous terms)
    if include_t and xs is not None:
        features.append(xs)
        names.append('t')
        for d in range(1, poly_degree + 1):
            features.append(xs * f**d)
            names.append(f't·f^{d}' if d > 1 else 't·f')

    # Log terms (Gompertz-type)
    log_f = np.log(np.clip(np.abs(f), 1e-12, None))
    features.append(f * log_f)
    names.append('f·ln|f|')
    features.append(f**2 * log_f)
    names.append('f²·ln|f|')

    Theta = np.column_stack(features)
    return Theta, names


def detect_nonlinear_ode(xs, ys, noise_level=None, poly_degree=2,
                          threshold=0.05, sigma_px=8):
    """
    Detect nonlinear ODE of the form f' = Θ(f,t)·c using STRidge.

    Returns (c_sparse, feature_names, residual, ode_str).
    """
    # Get smooth f and f'
    n = len(xs)
    s = 0 if (noise_level is None or noise_level == 0) else n * noise_level**2 * 2
    spl = UnivariateSpline(xs, ys, k=5, s=s)
    xs_u = np.linspace(xs[0], xs[-1], max(n, 200))
    ys_u = spl(xs_u)
    dx = xs_u[1] - xs_u[0]

    edge = int(np.ceil(3 * sigma_px))
    edge = min(edge, (len(xs_u) - 20) // 2)

    f_smooth  = gaussian_filter1d(ys_u, sigma_px, mode='nearest')
    f_prime   = gaussian_filter1d(ys_u, sigma_px, order=1, mode='nearest') / dx

    if edge > 0:
        xs_t    = xs_u[edge:-edge]
        f_smooth = f_smooth[edge:-edge]
        f_prime  = f_prime[edge:-edge]
    else:
        xs_t = xs_u

    # Build feature library
    Theta, names = build_nonlinear_feature_library(
        f_smooth, f_prime, poly_degree=poly_degree,
        include_t=True, xs=xs_t)

    # STRidge: find sparse c such that Theta @ c ≈ f_prime
    c = _stridge_regression(Theta, f_prime, threshold=threshold)

    pred = Theta @ c
    scale = np.std(f_prime) + 1e-30
    residual = float(np.sqrt(np.mean((pred - f_prime)**2)) / scale)

    # Build ODE string
    terms = [(names[i], c[i]) for i in range(len(c)) if abs(c[i]) > 0.01]
    ode_str = "f' = " + " + ".join(
        f"{v:.4f}·{n}" if abs(v-1.0)>1e-3 else n
        for n, v in terms) if terms else "f' = 0"

    return c, names, residual, ode_str


def _stridge_regression(Theta, y, threshold=0.05, max_iter=20, ridge=1e-5):
    """
    STRidge: iteratively solve least squares and threshold small coefficients.
    y ≈ Theta @ c, ||c||_0 minimised.
    """
    n_feat = Theta.shape[1]
    active = list(range(n_feat))
    c = np.zeros(n_feat)

    for _ in range(max_iter):
        if not active:
            break
        T_a = Theta[:, active]
        A = T_a.T @ T_a + ridge * np.eye(len(active))
        b = T_a.T @ y
        try:
            c_a = np.linalg.solve(A, b)
        except np.linalg.LinAlgError:
            break
        mask = np.abs(c_a) >= threshold * np.max(np.abs(c_a))
        if not np.any(mask):
            c_a = np.zeros(len(active))
            break
        active = [active[i] for i in range(len(active)) if mask[i]]
        c_a = c_a[mask]

    c_full = np.zeros(n_feat)
    for i, idx in enumerate(active):
        if i < len(c_a):
            c_full[idx] = c_a[i]
    return c_full


# ─────────────────────────────────────────────────────────────────────────────
# 5.  Parametric model fitting (known bacterial growth models)
# ─────────────────────────────────────────────────────────────────────────────

def logistic(t, y0, K, r):
    """y(t) = K / (1 + ((K-y0)/y0) * exp(-r*t))"""
    return K / (1 + ((K - y0) / y0) * np.exp(-r * t))

def gompertz(t, A, mu, lam):
    """Modified Gompertz (Zwietering 1990): y = A * exp(-exp(mu*e/A*(lam-t)+1))"""
    exponent = (mu * np.e / A) * (lam - t) + 1
    return A * np.exp(-np.exp(exponent))

def baranyi_roberts(t, y0, ymax, mu_max, lag):
    """Simplified Baranyi-Roberts model."""
    q0 = 1.0 / (np.exp(mu_max * lag) - 1 + 1e-10)
    h = t + (1.0 / mu_max) * np.log(
        (np.exp(-mu_max * t) + q0) / (1 + q0) + 1e-10)
    return ymax - np.log(1 + (np.exp(ymax - y0) - 1) * np.exp(-mu_max * h))

def richards(t, A, mu, lam, nu):
    """Richards (generalised logistic): adds shape parameter nu."""
    # Approximate: Gompertz is Richards with nu→0
    return A / (1 + nu * np.exp(-mu * (t - lam)))**(1/nu)

def fit_growth_models(xs, ys):
    """
    Fit all known bacterial growth models and return ranked results.
    Returns list of (name, params, rmse, r2, formula).
    """
    y0_est  = float(ys[0])
    K_est   = float(np.max(ys))
    t_range = float(xs[-1] - xs[0])

    results = []

    # ── Logistic
    try:
        p0 = [max(y0_est, 1e-6), K_est, 5.0 / t_range]
        bounds = ([0, K_est*0.5, 0], [K_est, K_est*2, 10/t_range])
        popt, _ = curve_fit(logistic, xs, ys, p0=p0, bounds=bounds,
                             maxfev=5000)
        y_pred = logistic(xs, *popt)
        rmse = float(np.sqrt(np.mean((ys - y_pred)**2)))
        r2   = float(1 - np.sum((ys-y_pred)**2)/np.sum((ys-np.mean(ys))**2))
        results.append({
            'model': 'Logistic',
            'params': dict(zip(['y0','K','r'], popt)),
            'rmse': rmse, 'r2': r2,
            'formula': f"K/(1+((K-y0)/y0)·exp(-r·t))  K={popt[1]:.4f} r={popt[2]:.4f} y0={popt[0]:.4f}",
            'y_pred': y_pred,
        })
    except Exception as e:
        results.append({'model': 'Logistic', 'rmse': np.inf, 'r2': -np.inf,
                        'error': str(e)})

    # ── Gompertz
    try:
        feats = detect_sigmoid_features(xs, ys)
        p0 = [K_est, feats['mu_max'], max(feats['lag'], 0.1)]
        bounds = ([K_est*0.3, 0, -t_range], [K_est*2, 10*feats['mu_max']+0.1, t_range])
        popt, _ = curve_fit(gompertz, xs, ys, p0=p0, bounds=bounds,
                             maxfev=5000)
        y_pred = gompertz(xs, *popt)
        rmse = float(np.sqrt(np.mean((ys - y_pred)**2)))
        r2   = float(1 - np.sum((ys-y_pred)**2)/np.sum((ys-np.mean(ys))**2))
        results.append({
            'model': 'Gompertz',
            'params': dict(zip(['A','mu','lambda'], popt)),
            'rmse': rmse, 'r2': r2,
            'formula': f"A·exp(-exp(μe/A·(λ-t)+1))  A={popt[0]:.4f} μ={popt[1]:.4f} λ={popt[2]:.4f}",
            'y_pred': y_pred,
        })
    except Exception as e:
        results.append({'model': 'Gompertz', 'rmse': np.inf, 'r2': -np.inf,
                        'error': str(e)})

    # ── Baranyi-Roberts (log-scale, typical for OD data)
    try:
        feats = detect_sigmoid_features(xs, ys)
        ys_log = np.log(np.clip(ys, 1e-10, None))
        y0_log = float(ys_log[0])
        ymax_log = float(np.max(ys_log))
        p0 = [y0_log, ymax_log, max(feats['mu_max']/np.mean(ys)*0.5, 0.01),
              max(feats['lag'], 0.1)]
        bounds = ([y0_log-5, ymax_log-2, 0, 0],
                  [y0_log+2, ymax_log+2, 5.0, t_range])
        popt, _ = curve_fit(baranyi_roberts, xs, ys_log, p0=p0,
                             bounds=bounds, maxfev=10000)
        y_pred_log = baranyi_roberts(xs, *popt)
        y_pred = np.exp(y_pred_log)
        rmse = float(np.sqrt(np.mean((ys - y_pred)**2)))
        r2   = float(1 - np.sum((ys-y_pred)**2)/np.sum((ys-np.mean(ys))**2))
        results.append({
            'model': 'Baranyi-Roberts (log)',
            'params': dict(zip(['y0','ymax','mu_max','lag'], popt)),
            'rmse': rmse, 'r2': r2,
            'formula': f"BR  y0={popt[0]:.4f} ymax={popt[1]:.4f} μ={popt[2]:.4f} lag={popt[3]:.4f}",
            'y_pred': y_pred,
        })
    except Exception as e:
        results.append({'model': 'Baranyi-Roberts', 'rmse': np.inf, 'r2': -np.inf,
                        'error': str(e)})

    results_valid = [r for r in results if r['rmse'] < np.inf]
    results_valid.sort(key=lambda r: r['rmse'])
    results_invalid = [r for r in results if r['rmse'] == np.inf]

    return results_valid + results_invalid


# ─────────────────────────────────────────────────────────────────────────────
# 6.  Main analysis pipeline for growth data
# ─────────────────────────────────────────────────────────────────────────────

def analyze_growth_curve(name, xs, ys, verbose=True):
    xs, ys = clean_xy(xs, ys)
    if verbose:
        print(f"\n{'='*62}")
        print(f"  {name}")
        print(f"{'='*62}")
        print(f"  n={len(xs)}, t∈[{xs[0]:.2f},{xs[-1]:.2f}], "
              f"y∈[{np.min(ys):.4f},{np.max(ys):.4f}]")

    # ── Step 1: Sigmoid feature detection
    feats = detect_sigmoid_features(xs, ys)
    if verbose:
        print(f"\n  Sigmoid features:")
        print(f"    y_min={feats['y_min']:.4f}  y_max={feats['y_max']:.4f}")
        print(f"    μ_max={feats['mu_max']:.4f}  lag={feats['lag']:.3f}")
        print(f"    inflection at t={feats['t_infl']:.2f}  "
              f"({feats['infl_rel']*100:.1f}% of range)")
        hint = "→ Gompertz-like" if feats['infl_rel'] < 0.4 else \
               "→ Logistic-like" if feats['infl_rel'] > 0.45 else \
               "→ symmetric sigmoid"
        print(f"    {hint}  monotone={feats['monotone']}  "
              f"saturating={feats['saturating']}")

    # ── Step 2: Transform-space linear ODE
    if verbose:
        print(f"\n  Transform-space linear ODE detection:")
    try:
        tr_results = detect_ode_in_transform(xs, ys)
        for r in tr_results[:3]:
            marker = " ✓" if r['residual'] < 0.05 else ""
            if verbose:
                print(f"    T={r['transform']:20s}  res={r['residual']:.3f}"
                      f"  ODE: {r['ode_str']}{marker}")
    except Exception as e:
        if verbose:
            print(f"    Failed: {e}")
        tr_results = []

    # ── Step 3: Nonlinear ODE detection
    if verbose:
        print(f"\n  Nonlinear ODE f' = Θ(f,t)·c:")
    try:
        c_nl, names, res_nl, ode_nl = detect_nonlinear_ode(xs, ys)
        if verbose:
            print(f"    Residual: {res_nl:.4f}")
            print(f"    {ode_nl}")
    except Exception as e:
        if verbose:
            print(f"    Failed: {e}")
        res_nl, ode_nl = np.inf, "—"

    # ── Step 4: Parametric model fitting
    if verbose:
        print(f"\n  Parametric model fit (RMSE / R²):")
    model_results = fit_growth_models(xs, ys)
    for r in model_results:
        if 'error' in r:
            if verbose:
                print(f"    {r['model']:30s}  FAILED: {r['error'][:40]}")
        else:
            marker = " ✓" if r['r2'] > 0.99 else ""
            if verbose:
                print(f"    {r['model']:30s}  RMSE={r['rmse']:.4f}  R²={r['r2']:.4f}"
                      f"{marker}")
                print(f"      {r['formula']}")

    # ── Summary
    best = model_results[0] if model_results and 'rmse' in model_results[0] else None
    if verbose and best and 'error' not in best:
        print(f"\n  Best parametric model: {best['model']}  "
              f"RMSE={best['rmse']:.4f}  R²={best['r2']:.4f}")

    return {
        'features': feats,
        'transform_ode': tr_results,
        'nonlinear_ode': (c_nl if 'c_nl' in dir() else None, ode_nl, res_nl),
        'model_fits': model_results,
    }


# ─────────────────────────────────────────────────────────────────────────────
# 7.  Load CSV and run
# ─────────────────────────────────────────────────────────────────────────────

def load_csv_column(path, col_idx=2):
    import csv
    xs, ys = [], []
    with open(path) as f:
        reader = csv.reader(f)
        next(reader)
        for row in reader:
            if len(row) <= col_idx:
                continue
            try:
                x = float(row[1])
                y = float(row[col_idx])
            except (ValueError, IndexError):
                continue
            xs.append(x)
            ys.append(y)
    return np.array(xs), np.array(ys)


if __name__ == '__main__':
    csv_path = "../pinheiroTech/curves-data/all_curves.csv"

    print("█"*62)
    print("  BACTERIAL GROWTH ODE ANALYSIS")
    print("█"*62)

    # Test on synthetic known models first
    t = np.linspace(0.1, 40, 150)

    print("\n── Synthetic: Logistic ──")
    ys_log = logistic(t, y0=0.05, K=1.0, r=0.3)
    analyze_growth_curve("Logistic (synthetic)", t, ys_log)

    print("\n── Synthetic: Gompertz ──")
    ys_gom = gompertz(t, A=1.0, mu=0.3, lam=5.0)
    analyze_growth_curve("Gompertz (synthetic)", t, ys_gom)

    print("\n── Synthetic: Baranyi-Roberts ──")
    ys_br = np.exp(baranyi_roberts(t, y0=np.log(0.05), ymax=np.log(1.2),
                                    mu_max=0.4, lag=4.0))
    analyze_growth_curve("Baranyi-Roberts (synthetic)", t, ys_br)

    # Real data
    print(f"\n{'█'*62}")
    print(f"  REAL DATA")
    print(f"{'█'*62}")
    try:
        for col in [2, 3, 4]:
            xs_r, ys_r = load_csv_column(csv_path, col_idx=col)
            if len(xs_r) > 10:
                analyze_growth_curve(f"Real curve col {col}", xs_r, ys_r)
    except FileNotFoundError:
        print(f"  CSV not found at {csv_path}")
        print("  (run from the Haskell-Genetic-Algorithm directory)")
