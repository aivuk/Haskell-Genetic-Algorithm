"""
Robust ODE Inversion for Symbolic Regression
=============================================
Improvements over explore_ode.py:
  1. Gaussian derivative filters instead of spline derivatives
     → convolves f with d^k/dx^k Gaussian: smooths + differentiates in one step
     → stable under noise (Gaussian acts as low-pass, then differentiates analytically)
  2. GCV (generalised cross-validation) for automatic bandwidth selection
  3. LASSO for sparse ODE detection (more robust than SVD null-space under noise)
  4. Integral / weak formulation as fallback (avoids differentiation entirely)
  5. Non-uniform data handling via spline resampling to uniform grid
"""

import numpy as np
from scipy.interpolate import UnivariateSpline
from scipy.ndimage import gaussian_filter1d
from scipy.signal import find_peaks
from scipy.optimize import minimize_scalar
import sympy as sp
from sympy import symbols, Function, dsolve, Eq
import sys
import warnings
warnings.filterwarnings('ignore')
sys.setrecursionlimit(20000)


# ─────────────────────────────────────────────────────────────────────────────
# 1.  Resample to uniform grid via spline (handles non-uniform input)
# ─────────────────────────────────────────────────────────────────────────────

def to_uniform_grid(xs, ys, n_out=None, noise_level=None):
    """
    Fit a smoothing spline and evaluate on a uniform grid.
    Returns (xs_uniform, ys_uniform).
    n_out: number of output points (default: same as input)
    """
    n = len(xs)
    n_out = n_out or n

    if noise_level is None or noise_level == 0.0:
        s = 0
    else:
        # Use 2x the naive smoothing to get smoother function for derivative estimation
        s = n * (noise_level ** 2) * 2.0

    spl = UnivariateSpline(xs, ys, k=5, s=s)
    xs_u = np.linspace(xs[0], xs[-1], n_out)
    ys_u = spl(xs_u)
    return xs_u, ys_u


# ─────────────────────────────────────────────────────────────────────────────
# 2.  GCV-optimal Gaussian filter bandwidth
# ─────────────────────────────────────────────────────────────────────────────

def gcv_bandwidth(ys_noisy, max_order=4, sigma_range=None, n_trials=40):
    """
    Choose Gaussian filter bandwidth (in pixels) via generalised cross-validation.

    Key constraint: sigma must be >= max_order * 2 pixels so that the k-th
    order Gaussian derivative kernel is properly resolved on the discrete grid
    (aliasing occurs for sigma < ~2-3 pixels for 2nd+ order derivatives).

    Returns (optimal_sigma_px, gcv_scores).
    """
    n = len(ys_noisy)
    # Minimum sigma: at least 2*max_order px to avoid aliasing of derivative kernels
    # Also at least 3 px for any derivative estimation
    sigma_min = max(3.0, float(max_order) * 2.0)
    sigma_max = sigma_range[1] if sigma_range else min(n / 4.0, 80.0)
    sigmas = np.logspace(np.log10(sigma_min), np.log10(sigma_max), n_trials)
    gcv_scores = []

    for sig in sigmas:
        ys_smooth = gaussian_filter1d(ys_noisy, sigma=sig)
        residuals = ys_noisy - ys_smooth
        # Hat diagonal h_ii ≈ 1/(sig*sqrt(2π)) for Gaussian smoother
        h = min(1.0 / (sig * np.sqrt(2 * np.pi)), 0.99)
        gcv = np.mean(residuals ** 2) / (1.0 - h) ** 2
        gcv_scores.append(gcv)

    best_idx = int(np.argmin(gcv_scores))
    return sigmas[best_idx], gcv_scores


# ─────────────────────────────────────────────────────────────────────────────
# 3.  Derivative matrix via Gaussian derivative filters
# ─────────────────────────────────────────────────────────────────────────────

def build_derivative_matrix_gaussian(xs_u, ys_u, max_order, sigma_px, trim=True):
    """
    D[i, j] = (d^j f / dx^j)(xs_u[i])  estimated via Gaussian derivative filter.

    trim=True: discard the first and last ceil(3*sigma_px) points, which are
    corrupted by boundary padding in the convolution. This is critical for
    clean derivative estimation without edge artefacts.
    """
    n = len(ys_u)
    dx = (xs_u[-1] - xs_u[0]) / (n - 1)
    edge = int(np.ceil(3.0 * sigma_px)) if trim else 0
    # Ensure at least 20 interior points remain
    edge = min(edge, (n - 20) // 2)
    edge = max(edge, 0)

    cols = []
    for k in range(max_order + 1):
        deriv_k = gaussian_filter1d(ys_u, sigma=sigma_px, order=k,
                                    mode='nearest') / (dx ** k)
        if edge > 0:
            deriv_k = deriv_k[edge:-edge]
        cols.append(deriv_k)
    return np.column_stack(cols), edge  # also return edge for xs trimming


# ─────────────────────────────────────────────────────────────────────────────
# 4.  Sparse ODE detection: SVD null-space + LASSO refinement
# ─────────────────────────────────────────────────────────────────────────────

def detect_polynomial_order(D, tol=1e-4):
    col_norms = np.linalg.norm(D, axis=0)
    scale = col_norms[0] + 1e-30
    for k in range(1, D.shape[1]):
        if col_norms[k] / scale < tol:
            return k, True
    return None, False


def detect_linear_ode_svd(D, tol=1e-3):
    """SVD null-space approach (works best for clean data)."""
    n_cols = D.shape[1]

    poly_order, is_poly = detect_polynomial_order(D, tol=tol)
    if is_poly:
        c = np.zeros(n_cols)
        c[poly_order] = 1.0
        return c, 0.0, poly_order

    col_norms = np.linalg.norm(D, axis=0) + 1e-30
    D_norm = D / col_norms
    _, S, Vt = np.linalg.svd(D_norm, full_matrices=True)

    c_norm = Vt[-1]
    c = c_norm / col_norms
    c /= (np.max(np.abs(c)) + 1e-30)

    residual = float(S[-1] / (S[0] + 1e-30))
    c_sparse = np.where(np.abs(c) > tol, c, 0.0)
    if np.any(c_sparse != 0):
        c_sparse /= np.max(np.abs(c_sparse))
    order = int(np.max(np.where(np.abs(c_sparse) > 0)[0])) if np.any(c_sparse != 0) else 0

    return c_sparse, residual, order


def detect_linear_ode_stridge(D, threshold=0.1, max_iter=20):
    """
    STRidge (Sequential Thresholded Ridge Regression) for sparse ODE detection.
    Much more reliable than LASSO pivot approach.

    For each possible pivot column k (fixing c[k]=1), solve:
        min ||D_other @ c_other + D[:,k]||^2  (ridge)
    then iteratively threshold small coefficients and re-solve.
    Pick the solution with fewest terms and smallest residual.

    This is the core algorithm from SINDy (Brunton et al. 2016).
    """
    n_cols = D.shape[1]
    col_norms = np.linalg.norm(D, axis=0) + 1e-30
    D_norm = D / col_norms

    best = None  # (residual, n_terms, c_sparse)

    for pivot in range(n_cols):
        rhs = -D_norm[:, pivot]
        cols = list(range(n_cols))
        cols.remove(pivot)
        active = cols.copy()

        c_active = np.zeros(len(cols))
        for _ in range(max_iter):
            if len(active) == 0:
                break
            X_active = D_norm[:, active]
            # Ridge regression (small regularisation for stability)
            A = X_active.T @ X_active + 1e-6 * np.eye(len(active))
            b = X_active.T @ rhs
            try:
                c_active = np.linalg.solve(A, b)
            except np.linalg.LinAlgError:
                break
            # Threshold: zero out small coefficients
            mask = np.abs(c_active) >= threshold
            if not np.any(mask):
                c_active = np.zeros(len(active))
                break
            active = [active[i] for i in range(len(active)) if mask[i]]
            c_active = c_active[mask]

        # Reconstruct full coefficient vector (normalised space)
        c_norm = np.zeros(n_cols)
        c_norm[pivot] = 1.0
        for i, col_i in enumerate(active):
            idx = cols.index(col_i)
            c_norm[col_i] = c_active[i] if i < len(c_active) else 0.0

        # Undo column normalisation
        c_full = c_norm / col_norms
        m = np.max(np.abs(c_full))
        if m < 1e-15:
            continue
        c_full /= m

        pred = D @ c_full
        scale = np.std(D[:, 0]) + 1e-30
        residual = float(np.sqrt(np.mean(pred ** 2)) / scale)
        n_terms = int(np.sum(np.abs(c_full) > 0.01))

        if best is None \
           or (residual < 0.02 and n_terms < best[1]) \
           or (n_terms == best[1] and residual < best[0]):
            best = (residual, n_terms, c_full.copy())

    if best is None:
        return np.zeros(n_cols), 1.0, 0

    res, _, c = best
    c_sparse = np.where(np.abs(c) > 0.01, c, 0.0)
    if np.any(c_sparse != 0):
        c_sparse /= np.max(np.abs(c_sparse))
    order = int(np.max(np.where(np.abs(c_sparse) > 0)[0])) if np.any(c_sparse != 0) else 0
    return c_sparse, res, order


# ─────────────────────────────────────────────────────────────────────────────
# 5.  Integral / weak formulation (most noise-robust, avoids differentiation)
# ─────────────────────────────────────────────────────────────────────────────

def build_integral_matrix(xs_u, ys_u, max_order, n_test=30):
    """
    Weak formulation: test the ODE c_0 f + c_1 f' + ... + 0 against
    compactly supported test functions phi_k, integrating by parts:

        ∫ c_j f^(j) phi_k dx = (-1)^j ∫ f phi_k^(j) dx  (+ boundary terms)

    So W[k, j] = (-1)^j ∫ ys_u(x) * phi_k^(j)(x) dx

    Uses Gaussian bumps as test functions, evaluated on the uniform grid.
    This requires only f (no derivatives of f!), so it's very noise-robust.
    """
    n = len(xs_u)
    dx = (xs_u[-1] - xs_u[0]) / (n - 1)

    # Place test function centres across the domain
    centres = np.linspace(xs_u[0] + 0.1*(xs_u[-1]-xs_u[0]),
                          xs_u[-1] - 0.1*(xs_u[-1]-xs_u[0]), n_test)
    sigma_phi = (xs_u[-1] - xs_u[0]) / (n_test * 0.5)

    W = np.zeros((n_test, max_order + 1))

    for k, mu in enumerate(centres):
        phi = np.exp(-0.5 * ((xs_u - mu) / sigma_phi) ** 2)
        for j in range(max_order + 1):
            # phi^(j): j-th derivative of Gaussian analytically
            phi_j = gaussian_filter1d(phi, sigma=1.0, order=j) / (1.0 ** j)
            # But use analytical formula for better accuracy:
            phi_j = _gaussian_deriv(xs_u, mu, sigma_phi, j)
            integrand = ys_u * phi_j
            W[k, j] = (-1) ** j * np.trapezoid(integrand, xs_u)

    return W


def _gaussian_deriv(x, mu, sigma, order):
    """Analytical k-th derivative of Gaussian bump."""
    u = (x - mu) / sigma
    g = np.exp(-0.5 * u ** 2) / (sigma * np.sqrt(2 * np.pi))
    if order == 0:
        return g
    # Use Hermite polynomial recurrence: G^(k) = (-1/sigma)^k * He_k(u) * G
    He = [np.ones_like(u), u]
    for k in range(2, order + 1):
        He.append(u * He[-1] - (k - 1) * He[-2])
    return (-1 / sigma) ** order * He[order] * g


# ─────────────────────────────────────────────────────────────────────────────
# 6.  Coefficient rounding and ODE string
# ─────────────────────────────────────────────────────────────────────────────

def round_ode_coefficients(c, threshold=0.05):
    c_clean = c.copy()
    for i, ci in enumerate(c):
        if abs(ci) < threshold:
            c_clean[i] = 0.0
        else:
            for target in [-2, -1, -0.5, 0.5, 1, 2]:
                if abs(ci - target) < threshold:
                    c_clean[i] = float(target)
                    break
    m = np.max(np.abs(c_clean))
    return c_clean / m if m > 0 else c_clean


def ode_to_string(c):
    terms = []
    names = ["f"] + ["f" + "'"*i if i <= 4 else f"f^({i})" for i in range(1, len(c))]
    for i, ci in enumerate(c):
        if abs(ci) < 1e-8:
            continue
        ci_r = round(ci, 4)
        if abs(ci_r - 1.0) < 1e-3:
            terms.append(names[i])
        elif abs(ci_r + 1.0) < 1e-3:
            terms.append(f"-{names[i]}")
        else:
            terms.append(f"{ci_r}*{names[i]}")
    return " + ".join(terms) + " = 0" if terms else "(trivial)"


# ─────────────────────────────────────────────────────────────────────────────
# 7.  Symbolic ODE solving
# ─────────────────────────────────────────────────────────────────────────────

def solve_ode_symbolically(c):
    x = symbols('x')
    f = Function('f')
    c_clean = round_ode_coefficients(c)
    terms = []
    for i, ci in enumerate(c_clean):
        if abs(ci) < 0.05:
            continue
        terms.append(sp.Rational(ci).limit_denominator(20) * f(x).diff(x, i))
    if not terms:
        return None
    ode_eq = Eq(sum(terms), 0)
    try:
        sol = dsolve(ode_eq, f(x))
        return sol
    except Exception as e:
        return None


# ─────────────────────────────────────────────────────────────────────────────
# 8.  Symmetry detection
# ─────────────────────────────────────────────────────────────────────────────

def detect_symmetries(xs, ys):
    """Returns dict of detected symmetries."""
    results = {}
    xmin, xmax = xs[0], xs[-1]

    # Parity (requires symmetric domain)
    xmax_sym = min(abs(xmin), abs(xmax)) * 0.9
    if xmax_sym > 0.1 * (xmax - xmin):
        spl = UnivariateSpline(xs, ys, k=5, s=0, ext=1)
        xs_test = np.linspace(0.05 * xmax_sym, xmax_sym, 100)
        fp = spl(xs_test)
        fn = spl(-xs_test)
        scale = np.mean(np.abs(fp)) + 1e-30
        err_even = np.mean(np.abs(fp - fn)) / scale
        err_odd  = np.mean(np.abs(fp + fn)) / scale
        if err_even < 0.05:
            results['parity'] = ('even', err_even)
        elif err_odd < 0.05:
            results['parity'] = ('odd', err_odd)
        else:
            results['parity'] = ('none', min(err_even, err_odd))

    # Periodicity via autocorrelation
    n = len(ys)
    ys_c = ys - np.mean(ys)
    acf = np.correlate(ys_c, ys_c, mode='full')[n-1:]
    acf /= (acf[0] + 1e-30)
    min_lag = max(5, n // 20)
    peaks, _ = find_peaks(acf[min_lag:], height=0.4, prominence=0.2)
    if len(peaks) > 0:
        lag = peaks[0] + min_lag
        period = (xmax - xmin) * lag / n
        results['period'] = (period, float(acf[lag]))
    else:
        results['period'] = (None, 0.0)

    # Scaling/homogeneity on positive domain
    xs_pos = xs[xs > 0]
    ys_pos = ys[xs > 0]
    if len(xs_pos) > 20:
        spl_pos = UnivariateSpline(xs_pos, ys_pos, k=5, s=0, ext=1)
        xs_t = np.linspace(xs_pos[0]*1.1, xs_pos[-1]*0.6, 80)
        log_f = np.log(np.abs(spl_pos(xs_t)) + 1e-30)
        ns = []
        for a in [1.5, 2.0, 2.5, 3.0]:
            log_fa = np.log(np.abs(spl_pos(a * xs_t)) + 1e-30)
            ns.append(np.median((log_fa - log_f) / np.log(a)))
        ns = np.array(ns)
        consistency = np.std(ns) / (np.abs(np.mean(ns)) + 0.1)
        if consistency < 0.1:
            results['scaling'] = (round(float(np.mean(ns)), 2), 1.0 - consistency)
        else:
            results['scaling'] = (None, 0.0)

    return results


# ─────────────────────────────────────────────────────────────────────────────
# 9.  Full pipeline
# ─────────────────────────────────────────────────────────────────────────────

def analyze(name, xs, ys, max_order=4, noise_std=0.0,
            use_weak=False, verbose=True):
    """
    Full pipeline:
      1. Resample to uniform grid
      2. GCV bandwidth selection
      3. Gaussian derivative matrix
      4. LASSO + SVD ODE detection
      5. Symbolic solution
      6. Symmetry detection
    """
    if verbose:
        print(f"\n{'='*62}")
        print(f"  {name}  (noise={noise_std:.3f})")
        print(f"{'='*62}")

    rng = np.random.default_rng(0)
    ys_obs = ys + rng.normal(0, noise_std, len(ys)) if noise_std > 0 else ys.copy()

    # ── Step 1: uniform grid
    noise_for_spline = noise_std if noise_std > 0 else None
    xs_u, ys_u = to_uniform_grid(xs, ys_obs, n_out=min(len(xs), 400),
                                  noise_level=noise_for_spline)
    dx = xs_u[1] - xs_u[0]

    # ── Step 2: GCV bandwidth
    sigma_px, _ = gcv_bandwidth(ys_u, max_order=max_order)
    if verbose:
        print(f"  GCV bandwidth: {sigma_px:.2f} px  ({sigma_px*dx:.4f} x-units)")

    # ── Step 3: derivative matrix (Gaussian filters)
    D, edge = build_derivative_matrix_gaussian(xs_u, ys_u, max_order, sigma_px)
    if verbose and edge > 0:
        print(f"  Edge trim: {edge} pts each side ({2*edge}/{len(xs_u)} removed)")

    # ── Step 4a: SVD
    c_svd, res_svd, ord_svd = detect_linear_ode_svd(D)
    if verbose:
        print(f"  SVD  residual={res_svd:.2e}  order={ord_svd}")
        print(f"  SVD  ODE: {ode_to_string(c_svd)}")

    # ── Step 4b: STRidge (better under noise)
    c_las, res_las, ord_las = detect_linear_ode_stridge(D)
    if verbose:
        print(f"  STRidge residual={res_las:.2e}  order={ord_las}")
        print(f"  STRidge ODE: {ode_to_string(c_las)}")

    # ── Step 4c: weak/integral formulation (if requested or both fail)
    c_weak = None
    if use_weak or (res_svd > 0.05 and res_las > 0.05):
        if verbose:
            print(f"  → trying weak/integral formulation (no differentiation)...")
        # Use full un-trimmed signal for integration
        W = build_integral_matrix(xs_u, ys_u, max_order)
        c_weak, res_weak, ord_weak = detect_linear_ode_svd(W)
        if verbose:
            print(f"  Weak residual={res_weak:.2e}  order={ord_weak}")
            print(f"  Weak ODE: {ode_to_string(c_weak)}")

    # ── Step 5: pick best result and solve symbolically
    candidates = [(res_svd, c_svd), (res_las, c_las)]

    if c_weak is not None:
        candidates.append((res_weak, c_weak))
    best_res, best_c = min(candidates, key=lambda t: t[0])

    sol = None
    if best_res < 0.05:
        if verbose:
            print(f"\n  ✓ ODE detected (residual {best_res:.2e}). Solving symbolically...")
        sol = solve_ode_symbolically(best_c)
        if sol is not None and verbose:
            print(f"  Solution: {sol}")
    else:
        if verbose:
            print(f"\n  ✗ No clean ODE found (best residual {best_res:.2e})")

    # ── Step 6: symmetries
    sym = detect_symmetries(xs_u, ys_u)
    if verbose:
        print(f"\n  Symmetries:")
        if 'parity' in sym:
            p, e = sym['parity']
            print(f"    Parity:      {p}  (err={e:.3f})")
        period, pconf = sym.get('period', (None, 0))
        if period:
            print(f"    Periodicity: T={period:.4f}  (conf={pconf:.3f})")
        else:
            print(f"    Periodicity: none")
        sn, sc = sym.get('scaling', (None, 0))
        if sn is not None:
            print(f"    Scaling:     n={sn}  (conf={sc:.3f})")

    return {'c_svd': c_svd, 'c_lasso': c_las, 'c_weak': c_weak,
            'best_c': best_c, 'best_res': best_res,
            'solution': sol, 'symmetries': sym}


# ─────────────────────────────────────────────────────────────────────────────
# 10.  Noise robustness sweep
# ─────────────────────────────────────────────────────────────────────────────

def noise_sweep(name, xs, ys_clean, noise_levels, max_order=4):
    print(f"\n{'#'*62}")
    print(f"  NOISE SWEEP: {name}")
    print(f"{'#'*62}")
    print(f"  {'Noise':>10}  {'SVD res':>10}  {'LASSO res':>10}  {'ODE found?':>12}  {'Solution'}")
    for sigma in noise_levels:
        r = analyze(name, xs, ys_clean, max_order=max_order,
                    noise_std=sigma, verbose=False)
        found = "YES" if r['best_res'] < 0.05 else "no"
        sol_str = str(r['solution'])[:50] if r['solution'] is not None else "—"
        print(f"  {sigma:>10.4f}  {r['c_svd'][1]:>10.2e}  {r['best_res']:>10.2e}"
              f"  {found:>12}  {sol_str}")


# ─────────────────────────────────────────────────────────────────────────────
# 11.  CSV loader and real-data test
# ─────────────────────────────────────────────────────────────────────────────

def load_csv_column(path, col_idx=2):
    import csv
    xs, ys = [], []
    with open(path) as f:
        reader = csv.reader(f)
        next(reader)  # skip header
        for row in reader:
            if len(row) <= col_idx:
                continue
            try:
                x = float(row[1])
                y = float(row[col_idx])
                xs.append(x)
                ys.append(y)
            except (ValueError, IndexError):
                continue
    return np.array(xs), np.array(ys)


# ─────────────────────────────────────────────────────────────────────────────
# 12.  Main: tests + real data
# ─────────────────────────────────────────────────────────────────────────────

if __name__ == '__main__':
    xs_full = np.linspace(-3*np.pi, 3*np.pi, 300)
    xs_pos  = np.linspace(0.1, 5.0, 200)
    xs_sym  = np.linspace(-4.0, 4.0, 300)

    # ── Clean-data sanity checks ──────────────────────────────────────────
    print("\n" + "█"*62)
    print("  CLEAN DATA TESTS")
    print("█"*62)
    analyze("sin(x)",        xs_full, np.sin(xs_full))
    analyze("exp(x)",        xs_pos,  np.exp(xs_pos))
    analyze("x²",            xs_sym,  xs_sym**2)
    analyze("x·sin(x)",      xs_full, xs_full*np.sin(xs_full), max_order=5)
    analyze("exp(x)·sin(x)", xs_pos,  np.exp(xs_pos)*np.sin(xs_pos), max_order=5)

    # ── Noise sweep ───────────────────────────────────────────────────────
    noise_sweep("sin(x)", xs_full, np.sin(xs_full),
                noise_levels=[0.0, 0.01, 0.02, 0.05, 0.10, 0.20, 0.50],
                max_order=4)

    noise_sweep("exp(x)", xs_pos, np.exp(xs_pos),
                noise_levels=[0.0, 0.01, 0.05, 0.10, 0.20],
                max_order=3)

    # ── Real data ─────────────────────────────────────────────────────────
    csv_path = "../pinheiroTech/curves-data/all_curves.csv"
    print(f"\n\n{'█'*62}")
    print(f"  REAL DATA: {csv_path}")
    print(f"{'█'*62}")

    try:
        xs_r, ys_r = load_csv_column(csv_path, col_idx=2)
        print(f"  Loaded {len(xs_r)} points,  x ∈ [{xs_r.min():.3f}, {xs_r.max():.3f}]"
              f",  y ∈ [{ys_r.min():.3f}, {ys_r.max():.3f}]")

        # Estimate noise level from high-frequency residual
        from scipy.signal import savgol_filter
        ys_smooth_est = savgol_filter(ys_r, window_length=min(21, len(ys_r)//5*2+1),
                                      polyorder=3)
        noise_est = float(np.std(ys_r - ys_smooth_est))
        snr = float(np.std(ys_r) / (noise_est + 1e-30))
        print(f"  Estimated noise σ ≈ {noise_est:.4f}  (SNR ≈ {snr:.1f})")

        # Run full pipeline
        analyze("real curve (col 2)", xs_r, ys_r,
                max_order=4, noise_std=0.0)   # treat as clean first
        analyze("real curve (col 2) [noisy]", xs_r, ys_r,
                max_order=4, noise_std=noise_est)  # with noise estimate

        # Also try columns 3 and 4 if available
        for col in [3, 4]:
            try:
                xs_c, ys_c = load_csv_column(csv_path, col_idx=col)
                if len(xs_c) > 20:
                    ys_sm = savgol_filter(ys_c, window_length=min(21,len(ys_c)//5*2+1),
                                         polyorder=3)
                    ne = float(np.std(ys_c - ys_sm))
                    print(f"\n  Column {col}: {len(xs_c)} pts,  noise σ≈{ne:.4f}")
                    analyze(f"real curve (col {col})", xs_c, ys_c, max_order=4)
            except Exception as e:
                print(f"  Column {col} failed: {e}")

    except FileNotFoundError:
        print(f"  CSV not found at {csv_path}")
        print(f"  Testing with synthetic noisy sin(x) as stand-in...")
        xs_r = np.linspace(-3*np.pi, 3*np.pi, 164)
        ys_r = np.sin(xs_r) + np.random.normal(0, 0.05, len(xs_r))
        analyze("synthetic stand-in", xs_r, ys_r, max_order=4, noise_std=0.05)
