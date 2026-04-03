"""
Step B: Improved nonlinear ODE detection for bacterial growth
=============================================================
Previous residual on col2: 0.39 (too high).

Strategy:
  B1. Work in log-space: detect ODE for g = ln(f)
      Gompertz: g' = c*(ln(A) - g) i.e. g' + c*g = const → very clean linear ODE
  B2. Direct Gompertz ODE scan: f' = r*f*ln(K/f), scan over K
  B3. Verify: does detected ODE predict held-out points?
"""

import numpy as np
from scipy.interpolate import UnivariateSpline
from scipy.ndimage import gaussian_filter1d
from scipy.optimize import curve_fit, minimize_scalar
import sys, os
sys.path.insert(0, os.path.dirname(__file__))

# ── helpers ───────────────────────────────────────────────────────────────────

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
                x = float(row[1]); y = float(row[col_idx])
            except (ValueError, IndexError):
                continue
            xs.append(x); ys.append(y)
    xs, ys = np.array(xs), np.array(ys)
    idx = np.argsort(xs); xs, ys = xs[idx], ys[idx]
    _, ui = np.unique(xs, return_index=True)
    return xs[ui], ys[ui]

def smooth_deriv(xs, ys, sigma_px=8, order=1, noise=None):
    n = len(xs)
    s = 0 if not noise else n * noise**2 * 2
    spl = UnivariateSpline(xs, ys, k=5, s=s)
    xu = np.linspace(xs[0], xs[-1], max(n, 300))
    yu = spl(xu)
    dx = xu[1] - xu[0]
    edge = min(int(np.ceil(3*sigma_px)), (len(xu)-20)//2)
    d = gaussian_filter1d(yu, sigma_px, order=order, mode='nearest') / dx**order
    return xu[edge:-edge], d[edge:-edge], yu[edge:-edge]

def rmse(y, yhat): return float(np.sqrt(np.mean((np.array(y)-np.array(yhat))**2)))
def r2(y, yhat):
    y = np.array(y); yhat = np.array(yhat)
    return float(1 - np.sum((y-yhat)**2)/np.sum((y-np.mean(y))**2))

# ── B1. Log-space linear ODE ──────────────────────────────────────────────────

def detect_log_space_ode(xs, ys, label=""):
    """
    Transform g = ln(f), then detect ODE for g via STRidge.
    Gompertz: g' + c*g = d  →  g' = d - c*g  (linear!)
    """
    print(f"\n  B1 — Log-space linear ODE  [{label}]")
    g = np.log(np.clip(ys, 1e-10, None))

    # Detect g' = c0 + c1*g using least squares (exact linear problem)
    xu, gp, gu = smooth_deriv(xs, g, sigma_px=8, order=1)
    # Interpolate g onto xu
    spl_g = UnivariateSpline(xs, g, k=5, s=0, ext=1)
    g_at_xu = spl_g(xu)

    # ODE: g' = a + b*g  →  [1, g] @ [a,b] = g'
    A = np.column_stack([np.ones_like(xu), g_at_xu])
    coeffs, _, _, _ = np.linalg.lstsq(A, gp, rcond=None)
    a, b = coeffs
    gp_pred = a + b * g_at_xu
    res = rmse(gp, gp_pred) / (np.std(gp) + 1e-10)

    print(f"    g' = {a:.4f} + {b:.4f}*g    residual={res:.4f}")

    if b < 0:
        # g' + |b|*g = a  →  solution: g(t) = a/|b| + C*exp(-|b|*t)
        # f(t) = exp(g) = exp(a/|b| + C*exp(-|b|*t))
        decay = -b
        asymptote_g = a / decay
        print(f"    → Gompertz-like: g → {asymptote_g:.4f} (K={np.exp(asymptote_g):.4f})")
        print(f"      decay rate c = {decay:.4f}")

        # Fit C from data: g(0) = a/|b| + C  → C = g(0) - a/|b|
        C_est = g[0] - asymptote_g
        t0 = xs[0]
        g_fitted = asymptote_g + C_est * np.exp(-decay * (xs - t0))
        f_fitted = np.exp(g_fitted)
        print(f"    Direct fit: RMSE={rmse(ys,f_fitted):.4f}  R²={r2(ys,f_fitted):.4f}")

        # Better: fit C numerically
        def loss(C): return rmse(ys, np.exp(asymptote_g + C*np.exp(-decay*(xs-t0))))
        res_opt = minimize_scalar(loss, bounds=(-20, 0), method='bounded')
        C_opt = res_opt.x
        f_opt = np.exp(asymptote_g + C_opt*np.exp(-decay*(xs-t0)))
        print(f"    Optimised C: RMSE={rmse(ys,f_opt):.4f}  R²={r2(ys,f_opt):.4f}")

        return {'a': a, 'b': b, 'res': res,
                'rmse': rmse(ys, f_opt), 'r2': r2(ys, f_opt),
                'C': C_opt, 'K': np.exp(asymptote_g), 'decay': decay}
    else:
        print(f"    → Not Gompertz-like (b > 0, diverging)")
        return {'a': a, 'b': b, 'res': res, 'rmse': np.inf, 'r2': -np.inf}


# ── B2. Direct Gompertz ODE scan: f' = r*f*ln(K/f) ───────────────────────────

def detect_gompertz_ode_direct(xs, ys, label=""):
    """
    Gompertz ODE:  f' = r * f * ln(K/f)  = r * f * (ln(K) - ln(f))
    For a fixed K, this is linear in the feature [f*ln(K/f)]:
        f' = r * [f * ln(K/f)]
    So we scan K values and solve r = (f' · Φ) / (Φ · Φ)
    where Φ = f * ln(K/f).
    Best K minimises residual ||f' - r*Φ||.
    """
    print(f"\n  B2 — Direct Gompertz ODE scan  [{label}]")
    xu, fp, fu = smooth_deriv(xs, ys, sigma_px=8, order=1)
    spl = UnivariateSpline(xs, ys, k=5, s=0, ext=1)
    f_at_xu = spl(xu)

    K_max = np.max(ys) * 3
    K_min = np.max(ys) * 1.001
    best = {'res': np.inf}

    for K in np.linspace(K_min, K_max, 80):
        phi = f_at_xu * np.log(K / np.clip(f_at_xu, 1e-12, None))
        phi_phi = np.dot(phi, phi)
        if phi_phi < 1e-10: continue
        r = np.dot(fp, phi) / phi_phi
        fp_pred = r * phi
        res = rmse(fp, fp_pred) / (np.std(fp) + 1e-10)
        if res < best['res']:
            best = {'res': res, 'K': K, 'r': r}

    K_opt, r_opt = best['K'], best['r']
    print(f"    Best K={K_opt:.4f}  r={r_opt:.4f}  residual={best['res']:.4f}")

    # Fit the Gompertz model with these parameters as initial guess
    def gompertz_ode_fit(t, A, mu, lam):
        return A * np.exp(-np.exp((mu*np.e/A)*(lam-t)+1))
    try:
        popt, _ = curve_fit(gompertz_ode_fit, xs, ys,
                            p0=[K_opt, r_opt, xs[len(xs)//3]],
                            bounds=([K_min*0.5, 0, -xs[-1]],
                                    [K_max, 5.0, xs[-1]]),
                            maxfev=10000)
        yp = gompertz_ode_fit(xs, *popt)
        print(f"    Refined: A={popt[0]:.4f} μ={popt[1]:.4f} λ={popt[2]:.4f}")
        print(f"    RMSE={rmse(ys,yp):.4f}  R²={r2(ys,yp):.4f}")
        return {'K': popt[0], 'r': popt[1], 'lam': popt[2],
                'rmse': rmse(ys,yp), 'r2': r2(ys,yp)}
    except Exception as e:
        print(f"    Refinement failed: {e}")
        return best


# ── B3. Extended feature library (SINDy-style for growth) ─────────────────────

def detect_nonlinear_ode_growth(xs, ys, label=""):
    """
    Build targeted feature library for bacterial growth:
    f' ≈ c0 + c1*f + c2*f^2 + c3*f*ln(f) + c4*f*ln(f)^2
    (covers logistic, Gompertz, and intermediate models)
    Use STRidge to find sparse coefficients.
    """
    print(f"\n  B3 — Growth-targeted nonlinear ODE  [{label}]")
    xu, fp, fu = smooth_deriv(xs, ys, sigma_px=8, order=1)
    spl = UnivariateSpline(xs, ys, k=5, s=0, ext=1)
    f = spl(xu)
    lf = np.log(np.clip(np.abs(f), 1e-12, None))

    # Feature library focused on growth ODEs
    Theta = np.column_stack([
        np.ones_like(f),     # constant
        f,                   # linear (exponential growth)
        f**2,                # logistic term
        f * lf,              # Gompertz-type: f*ln(f)
        f * (1 - f/np.max(f)),  # explicit logistic: f*(1-f/K)
    ])
    names = ['1', 'f', 'f²', 'f·ln(f)', 'f·(1-f/K)']

    # Simple least squares first (no sparsity)
    coeffs, _, _, _ = np.linalg.lstsq(Theta, fp, rcond=None)
    fp_pred = Theta @ coeffs
    res_ls = rmse(fp, fp_pred) / (np.std(fp)+1e-10)

    # STRidge for sparsity
    def stridge(Th, y, thr=0.1, n_iter=20):
        n = Th.shape[1]
        active = list(range(n))
        c = np.zeros(n)
        for _ in range(n_iter):
            if not active: break
            Ta = Th[:, active]
            c_a = np.linalg.lstsq(Ta.T@Ta + 1e-6*np.eye(len(active)),
                                   Ta.T@y, rcond=None)[0]
            mask = np.abs(c_a) >= thr * np.max(np.abs(c_a))
            if not np.any(mask): break
            active = [active[i] for i in range(len(active)) if mask[i]]
            c_a = c_a[mask]
        c_full = np.zeros(n)
        for i, idx in enumerate(active):
            if i < len(c_a): c_full[idx] = c_a[i]
        return c_full

    c_sparse = stridge(Theta, fp, thr=0.1)
    fp_sparse = Theta @ c_sparse
    res_sparse = rmse(fp, fp_sparse) / (np.std(fp)+1e-10)

    terms_ls = [(names[i], coeffs[i]) for i in range(len(coeffs)) if abs(coeffs[i])>1e-4]
    terms_sp = [(names[i], c_sparse[i]) for i in range(len(c_sparse)) if abs(c_sparse[i])>1e-4]

    print(f"    Full LS:  f' = " + " + ".join(f"{v:.4f}·{n}" for n,v in terms_ls))
    print(f"    residual={res_ls:.4f}")
    print(f"    Sparse:   f' = " + (" + ".join(f"{v:.4f}·{n}" for n,v in terms_sp) or "0"))
    print(f"    residual={res_sparse:.4f}")

    # Identify model type from sparse coefficients
    c_dict = {names[i]: c_sparse[i] for i in range(len(c_sparse))}
    if abs(c_dict.get('f·ln(f)', 0)) > abs(c_dict.get('f²', 0)):
        print(f"    → Gompertz signature (f·ln(f) dominant)")
    elif abs(c_dict.get('f²', 0)) > 0.01:
        print(f"    → Logistic signature (f² dominant)")
    elif abs(c_dict.get('f', 0)) > 0.01:
        print(f"    → Exponential growth (f dominant)")

    return {'res_ls': res_ls, 'res_sparse': res_sparse,
            'coeffs': c_dict}


# ── Main ──────────────────────────────────────────────────────────────────────

if __name__ == '__main__':
    csv_path = "../pinheiroTech/curves-data/all_curves.csv"
    print("="*60)
    print("  STEP B: Improved Nonlinear ODE Detection")
    print("="*60)

    results = {}
    for col in [2, 3, 4]:
        print(f"\n{'█'*60}")
        print(f"  Column {col}")
        print(f"{'█'*60}")
        xs, ys = load_csv_column(csv_path, col_idx=col)
        r_b1 = detect_log_space_ode(xs, ys, f"col{col}")
        r_b2 = detect_gompertz_ode_direct(xs, ys, f"col{col}")
        r_b3 = detect_nonlinear_ode_growth(xs, ys, f"col{col}")
        results[col] = {'B1': r_b1, 'B2': r_b2, 'B3': r_b3}

    # Synthetic validation
    print(f"\n\n{'='*60}")
    print("  SYNTHETIC VALIDATION (known Gompertz)")
    print("="*60)
    t = np.linspace(0.5, 40, 150)
    for A, mu, lam in [(1.0, 0.3, 5.0), (1.2, 0.5, 8.0)]:
        ys_syn = A * np.exp(-np.exp((mu*np.e/A)*(lam-t)+1))
        label = f"A={A} μ={mu} λ={lam}"
        print(f"\n── {label} ──")
        detect_log_space_ode(t, ys_syn, label)
        detect_gompertz_ode_direct(t, ys_syn, label)
        detect_nonlinear_ode_growth(t, ys_syn, label)

    print("\nStep B complete.")
