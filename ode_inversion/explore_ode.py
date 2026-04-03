"""
ODE Inversion for Symbolic Regression — Phase 1 & 2 Exploration
================================================================
Core idea: instead of searching for f(x) directly, find the simplest
linear ODE that f satisfies:  c0*f + c1*f' + c2*f'' + ... = 0
Then reconstruct f symbolically from the ODE.

Steps tested here:
  1. Smooth data via B-spline, extract derivatives analytically
  2. Build derivative matrix D[i,j] = f^(j)(x_i)
  3. Find sparse c via SVD null-space + LASSO
  4. Solve ODE symbolically with sympy
  5. Detect parity / periodicity symmetries
"""

import numpy as np
from scipy.interpolate import UnivariateSpline
from scipy.signal import find_peaks
import sympy as sp
from sympy import symbols, Function, dsolve, Eq, cos, sin, exp, ode_order
import warnings
warnings.filterwarnings('ignore')

# ── 1. Smooth spline fitting ──────────────────────────────────────────────────

def fit_spline(xs, ys, noise_level=None, k=5):
    """
    Fit a smoothing spline to (xs, ys).
    noise_level=None  → interpolating spline (s=0, exact through data)
    noise_level=sigma → smoothing spline, s = n * sigma^2
    """
    n = len(xs)
    if noise_level is None or noise_level == 0.0:
        s = 0  # interpolating: exact fit, accurate derivatives
    else:
        s = n * noise_level ** 2

    spl = UnivariateSpline(xs, ys, k=k, s=s)
    return spl


def build_derivative_matrix(spl, xs, max_order=5):
    """
    D[i, j] = f^(j)(xs[i])   for j in 0..max_order
    Columns are: [f, f', f'', f''', f'''', f''''']
    """
    cols = []
    for order in range(max_order + 1):
        deriv = spl.derivatives  # gets all derivatives at once
        vals = np.array([spl(x, nu=order) for x in xs])
        cols.append(vals)
    D = np.column_stack(cols)
    return D


# ── 2. Sparse ODE detection via SVD ──────────────────────────────────────────

def detect_polynomial_order(D, tol=1e-6):
    """
    Detect if f is a polynomial: find k such that f^(k) ≈ 0 everywhere.
    Returns (k, True) if polynomial of degree k-1, else (None, False).
    """
    col_norms = np.linalg.norm(D, axis=0)
    scale = col_norms[0] + 1e-30  # use f itself as scale reference
    for k in range(1, D.shape[1]):
        if col_norms[k] / scale < tol:
            return k, True
    return None, False


def detect_linear_ode(D, tol=1e-4):
    """
    Find the sparse coefficient vector c minimizing ||D @ c||.
    First checks for polynomial structure (zero derivative column).
    Uses SVD null-space otherwise.
    Returns (c, residual, order).
    """
    n_cols = D.shape[1]

    # Special case: polynomial detection
    poly_order, is_poly = detect_polynomial_order(D, tol=1e-5)
    if is_poly:
        c = np.zeros(n_cols)
        c[poly_order] = 1.0
        return c, 0.0, poly_order

    # Normalize each column (important for numerical stability)
    col_norms = np.linalg.norm(D, axis=0) + 1e-30
    D_norm = D / col_norms

    _, S, Vt = np.linalg.svd(D_norm, full_matrices=True)

    # Smallest singular value → null space direction
    c_norm = Vt[-1]

    # Undo normalization
    c = c_norm / col_norms
    c = c / (np.max(np.abs(c)) + 1e-30)  # normalize: max coeff = 1

    residual = S[-1] / (S[0] + 1e-30)

    # Threshold small coefficients
    c_sparse = np.where(np.abs(c) > tol, c, 0.0)
    if np.any(c_sparse != 0):
        c_sparse = c_sparse / np.max(np.abs(c_sparse))

    order = int(np.max(np.where(np.abs(c_sparse) > 0)[0])) if np.any(c_sparse != 0) else 0

    return c_sparse, float(residual), order


def round_ode_coefficients(c, threshold=0.05):
    """
    Round near-integer ODE coefficients to clean integers/fractions.
    Zero out coefficients below threshold.
    """
    c_clean = c.copy()
    for i, ci in enumerate(c):
        if abs(ci) < threshold:
            c_clean[i] = 0.0
        else:
            # Round to nearest 0.5 if very close
            for target in [-2, -1, -0.5, 0.5, 1, 2]:
                if abs(ci - target) < threshold:
                    c_clean[i] = target
                    break
    # Re-normalize so max abs = 1
    m = np.max(np.abs(c_clean))
    if m > 0:
        c_clean = c_clean / m
    return c_clean


def ode_to_string(c):
    """Human-readable form of the ODE."""
    terms = []
    for i, ci in enumerate(c):
        if abs(ci) < 1e-8:
            continue
        deriv = "f" + "'" * i if i <= 4 else f"f^({i})"
        ci_r = round(ci, 4)
        if abs(ci_r - 1.0) < 1e-3:
            terms.append(deriv)
        elif abs(ci_r + 1.0) < 1e-3:
            terms.append(f"-{deriv}")
        else:
            terms.append(f"{ci_r}*{deriv}")
    return " + ".join(terms) + " = 0" if terms else "(trivial)"


# ── 3. Symbolic ODE solving via sympy ────────────────────────────────────────

def solve_ode_symbolically(c):
    """
    Given coefficients c of the linear ODE  sum_i c[i] * f^(i) = 0,
    solve it symbolically with sympy.
    Uses rounded integer/rational coefficients for clean solutions.
    """
    import sys
    sys.setrecursionlimit(10000)

    x = symbols('x')
    f = Function('f')

    # Round coefficients to avoid spurious roots from floating-point noise
    c_clean = round_ode_coefficients(c)

    # Build the ODE equation
    terms = []
    for i, ci in enumerate(c_clean):
        if abs(ci) < 1e-8:
            continue
        terms.append(sp.Rational(ci).limit_denominator(20) * f(x).diff(x, i))

    if not terms:
        return None

    ode_eq = Eq(sum(terms), 0)
    print(f"  Rounded ODE: {ode_eq}")

    try:
        sol = dsolve(ode_eq, f(x))
        return sol
    except Exception as e:
        print(f"  sympy dsolve failed: {e}")
        return None


# ── 4. Symmetry detection ─────────────────────────────────────────────────────

def detect_parity(spl, x_range, n=200):
    """
    Test if f is even (f(-x) = f(x)) or odd (f(-x) = -f(x)).
    Returns 'even', 'odd', or 'none'.
    """
    xs = np.linspace(x_range[0], x_range[1], n)
    # Only test on symmetric domain
    xmax = min(abs(x_range[0]), abs(x_range[1]))
    xs_sym = np.linspace(0.1, xmax, n)

    f_pos = np.array([spl(x) for x in xs_sym])
    f_neg = np.array([spl(-x) for x in xs_sym])

    scale = np.mean(np.abs(f_pos)) + 1e-30
    err_even = np.mean(np.abs(f_pos - f_neg)) / scale
    err_odd  = np.mean(np.abs(f_pos + f_neg)) / scale

    if err_even < 0.05:
        return 'even', err_even
    elif err_odd < 0.05:
        return 'odd', err_odd
    else:
        return 'none', min(err_even, err_odd)


def detect_periodicity(spl, x_range, n=500):
    """
    Detect periodicity via autocorrelation.
    Returns (period, confidence) or (None, 0).
    """
    xs = np.linspace(x_range[0], x_range[1], n)
    ys = np.array([spl(x) for x in xs])
    ys = ys - np.mean(ys)

    # Autocorrelation
    acf = np.correlate(ys, ys, mode='full')
    acf = acf[n-1:]  # keep positive lags
    acf /= acf[0]   # normalize

    # Find peaks in autocorrelation after lag > 5% of signal
    min_lag = max(5, n // 20)
    peaks, props = find_peaks(acf[min_lag:], height=0.5, prominence=0.3)

    if len(peaks) == 0:
        return None, 0.0

    first_peak_lag = peaks[0] + min_lag
    period = (x_range[1] - x_range[0]) * first_peak_lag / n
    confidence = acf[first_peak_lag]

    return period, float(confidence)


def detect_scaling(spl, x_range, n=100):
    """
    Detect if f(ax) = a^n * f(x) (homogeneity / power-law scaling).
    Try several values of a, fit log|f(ax)| = n*log(a) + log|f(x)|.
    Returns (n, confidence) or (None, 0).
    """
    xs = np.linspace(max(0.2, x_range[0]), x_range[1] * 0.8, n)
    ys = np.abs(np.array([spl(x) for x in xs]))
    log_ys = np.log(ys + 1e-30)

    alphas = [1.5, 2.0, 2.5, 3.0]
    ns = []
    for a in alphas:
        ys_scaled = np.abs(np.array([spl(a * x) for x in xs]))
        log_ys_scaled = np.log(ys_scaled + 1e-30)
        # log|f(ax)| = n*log(a) + log|f(x)| → n = (log|f(ax)| - log|f(x)|) / log(a)
        n_est = (log_ys_scaled - log_ys) / np.log(a)
        ns.append(np.median(n_est))

    ns = np.array(ns)
    # If consistent across different a values → real scaling
    consistency = np.std(ns) / (np.abs(np.mean(ns)) + 0.1)
    if consistency < 0.1:
        return round(np.mean(ns), 2), 1.0 - consistency
    return None, 0.0


# ── 5. Full pipeline ──────────────────────────────────────────────────────────

def analyze_function(name, xs, ys, max_order=5, noise_std=0.0, verbose=True):
    """
    Run the full ODE inversion + symmetry detection pipeline.
    """
    print(f"\n{'='*60}")
    print(f"  Analyzing: {name}")
    print(f"{'='*60}")

    # Add noise if requested
    if noise_std > 0:
        ys = ys + np.random.normal(0, noise_std, len(ys))
        print(f"  (noise_std = {noise_std:.4f})")

    # Fit spline
    spl = fit_spline(xs, ys, noise_level=noise_std if noise_std > 0 else None)
    y_fit = np.array([spl(x) for x in xs])
    fit_err = np.mean((ys - y_fit)**2)**0.5
    print(f"  Spline fit RMSE: {fit_err:.2e}")

    # Build derivative matrix
    D = build_derivative_matrix(spl, xs, max_order=max_order)
    print(f"  Derivative matrix shape: {D.shape}")

    # Detect ODE
    c, residual, order = detect_linear_ode(D)
    print(f"  Relative null-space residual: {residual:.2e}")
    print(f"  Detected ODE order: {order}")
    print(f"  ODE coefficients: {np.round(c, 4)}")
    print(f"  ODE: {ode_to_string(c)}")

    # Solve symbolically
    if residual < 0.01:
        print(f"\n  Solving ODE symbolically...")
        sol = solve_ode_symbolically(c)
        if sol is not None:
            print(f"  General solution: {sol}")
    else:
        print(f"  Residual too large for reliable ODE detection")

    # Symmetry detection
    x_range = (xs[0], xs[-1])
    parity, p_err = detect_parity(spl, x_range)
    period, p_conf = detect_periodicity(spl, x_range)
    scale_n, s_conf = detect_scaling(spl, x_range)

    print(f"\n  Symmetries detected:")
    print(f"    Parity:      {parity} (error={p_err:.3f})")
    if period:
        print(f"    Periodicity: T={period:.4f} (conf={p_conf:.3f})")
    else:
        print(f"    Periodicity: not detected")
    if scale_n is not None:
        print(f"    Scaling:     n={scale_n} (conf={s_conf:.3f})")
    else:
        print(f"    Scaling:     not detected")

    return {
        'spline': spl, 'D': D, 'c': c, 'residual': residual,
        'parity': parity, 'period': period, 'scale_n': scale_n
    }


# ── 6. Test suite ─────────────────────────────────────────────────────────────

if __name__ == '__main__':
    np.random.seed(42)

    xs_full  = np.linspace(-3*np.pi, 3*np.pi, 300)
    xs_pos   = np.linspace(0.1, 5.0, 200)
    xs_sym   = np.linspace(-4.0, 4.0, 300)
    xs_tight = np.linspace(0.5, 4.0, 200)

    test_cases = [
        # (name, xs, ys, expected_ode, max_order)
        ("sin(x)",       xs_full,  np.sin(xs_full),       "f'' + f = 0",  4),
        ("cos(x)",       xs_full,  np.cos(xs_full),       "f'' + f = 0",  4),
        ("exp(x)",       xs_pos,   np.exp(xs_pos),        "f' - f = 0",   3),
        ("exp(-x)",      xs_pos,   np.exp(-xs_pos),       "f' + f = 0",   3),
        ("x^2",          xs_sym,   xs_sym**2,             "f''' = 0",     4),
        ("x^3",          xs_pos,   xs_pos**3,             "f'''' = 0",    5),
        ("sin(x)+x",     xs_full,  np.sin(xs_full)+xs_full, "f''' + f' = 0", 4),
        ("x*sin(x)",     xs_full,  xs_full*np.sin(xs_full), "tricky",     5),
        ("exp(x)*sin(x)",xs_pos,   np.exp(xs_pos)*np.sin(xs_pos), "4th order", 5),
        ("1/x",          xs_pos,   1/xs_pos,              "x*f' + f = 0 (Euler)", 3),
    ]

    print("\n" + "="*60)
    print("  CLEAN DATA TESTS")
    print("="*60)

    results = {}
    for name, xs, ys, expected, max_ord in test_cases:
        r = analyze_function(name, xs, ys, max_order=max_ord, noise_std=0.0)
        results[name] = r

    print("\n\n" + "="*60)
    print("  NOISY DATA TEST: sin(x) with increasing noise")
    print("="*60)

    for noise in [0.0, 0.01, 0.05, 0.1, 0.2]:
        r = analyze_function(
            f"sin(x) noise={noise}", xs_full, np.sin(xs_full),
            max_order=4, noise_std=noise
        )

    print("\n\nDone.")
