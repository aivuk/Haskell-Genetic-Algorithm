"""
Step A: Fit constants from ODE solution class
==============================================
After the ODE inversion detects the solution class (e.g. {1, x, sin(x/2), cos(x/2)}),
fit C1..Cn via linear least squares and compare MSE vs parametric Gompertz fit.

Also tests: for bacterial growth (nonlinear), does the linear-ODE solution class
give a meaningful fit, or does it fundamentally fail?
"""

import numpy as np
from scipy.interpolate import UnivariateSpline
from scipy.optimize import curve_fit
import sys, os
sys.path.insert(0, os.path.dirname(__file__))

# ── helpers ──────────────────────────────────────────────────────────────────

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
    order = np.argsort(xs)
    xs, ys = xs[order], ys[order]
    _, ui = np.unique(xs, return_index=True)
    return xs[ui], ys[ui]

def rmse(y, yhat): return float(np.sqrt(np.mean((y - yhat)**2)))
def r2(y, yhat):   return float(1 - np.sum((y-yhat)**2)/np.sum((y-np.mean(y))**2))

# ── parametric Gompertz (reference) ──────────────────────────────────────────

def gompertz(t, A, mu, lam):
    return A * np.exp(-np.exp((mu * np.e / A) * (lam - t) + 1))

def fit_gompertz(xs, ys):
    K = np.max(ys); mu0 = 0.3; lag = xs[len(xs)//3]
    try:
        popt, _ = curve_fit(gompertz, xs, ys,
                            p0=[K, mu0, lag],
                            bounds=([K*0.5,0,-xs[-1]], [K*2, 5, xs[-1]]),
                            maxfev=10000)
        yp = gompertz(xs, *popt)
        return popt, rmse(ys, yp), r2(ys, yp)
    except Exception as e:
        return None, np.inf, -np.inf

# ── Step A-1: linear solution-class fitting ───────────────────────────────────

def fit_solution_class(xs, ys, basis_fns, basis_names):
    """
    Fit  y ≈ C1*phi1(x) + C2*phi2(x) + ...  via linear least squares.
    basis_fns: list of callables phi_i(xs) -> array
    Returns (coeffs, y_pred, rmse, r2).
    """
    Phi = np.column_stack([phi(xs) for phi in basis_fns])
    coeffs, _, _, _ = np.linalg.lstsq(Phi, ys, rcond=None)
    y_pred = Phi @ coeffs
    named = {basis_names[i]: coeffs[i] for i in range(len(coeffs))}
    return named, y_pred, rmse(ys, y_pred), r2(ys, y_pred)


# ── Step A-2: try several plausible solution classes ─────────────────────────

def evaluate_all_classes(xs, ys, label):
    print(f"\n{'─'*60}")
    print(f"  {label}")
    print(f"{'─'*60}")

    # Reference: Gompertz parametric
    gp, grmse, gr2 = fit_gompertz(xs, ys)
    print(f"  Gompertz (reference):      RMSE={grmse:.4f}  R²={gr2:.4f}")
    if gp is not None:
        print(f"    A={gp[0]:.4f}  μ={gp[1]:.4f}  λ={gp[2]:.4f}")

    # ── Class 1: what linear ODE inversion found (oscillatory — wrong for growth)
    omega = 0.5   # from detected ODE  f'''' + 0.255*f'' = 0
    basis1 = [lambda x: np.ones_like(x),
              lambda x: x,
              lambda x, w=omega: np.sin(w*x),
              lambda x, w=omega: np.cos(w*x)]
    names1 = ["1", "x", f"sin({omega}x)", f"cos({omega}x)"]
    c1, yp1, r1, rr1 = fit_solution_class(xs, ys, basis1, names1)
    print(f"\n  Linear ODE class (oscillatory):")
    print(f"    Basis: {names1}")
    print(f"    Coefficients: { {k: f'{v:.4f}' for k,v in c1.items()} }")
    print(f"    RMSE={r1:.4f}  R²={rr1:.4f}")

    # ── Class 2: Gompertz solution class in log space
    # ln(f) ~ a + b*exp(-c*x)  →  f ~ exp(a + b*exp(-c*x))
    # Linearise: fit ln(f) = a + b*exp(-c*x) by scanning c
    log_ys = np.log(np.clip(ys, 1e-10, None))
    best_c = {'rmse': np.inf}
    for c_val in np.linspace(0.05, 1.0, 40):
        basis = [lambda x: np.ones_like(x),
                 lambda x, c=c_val: np.exp(-c*x)]
        coeffs, _, _, _ = np.linalg.lstsq(
            np.column_stack([b(xs) for b in basis]), log_ys, rcond=None)
        log_pred = coeffs[0] + coeffs[1]*np.exp(-c_val*xs)
        y_pred = np.exp(log_pred)
        err = rmse(ys, y_pred)
        if err < best_c['rmse']:
            best_c = {'rmse': err, 'r2': r2(ys,y_pred),
                      'c': c_val, 'a': coeffs[0], 'b': coeffs[1]}
    print(f"\n  Gompertz log-linear class (f=exp(a+b*exp(-c*x))):")
    print(f"    Best c={best_c['c']:.4f}  a={best_c['a']:.4f}  b={best_c['b']:.4f}")
    print(f"    RMSE={best_c['rmse']:.4f}  R²={best_c['r2']:.4f}")
    yp_gomp_log = np.exp(best_c['a'] + best_c['b']*np.exp(-best_c['c']*xs))

    # ── Class 3: logistic class  f = K / (1 + exp(-r*(x-x0)))
    best_logist = {'rmse': np.inf}
    K_est = np.max(ys) * 1.02
    for r_val in np.linspace(0.05, 2.0, 40):
        x0s = xs[np.argmin(np.abs(ys - K_est/2))]
        basis = [lambda x, rv=r_val, x0=x0s: 1/(1 + np.exp(-rv*(x-x0)))]
        coeffs, _, _, _ = np.linalg.lstsq(
            np.column_stack([b(xs) for b in basis]), ys, rcond=None)
        y_pred = coeffs[0] / (1 + np.exp(-r_val*(xs-x0s)))
        err = rmse(ys, y_pred)
        if err < best_logist['rmse']:
            best_logist = {'rmse': err, 'r2': r2(ys,y_pred),
                           'r': r_val, 'x0': x0s, 'K': coeffs[0]}
    print(f"\n  Logistic class:")
    print(f"    K={best_logist['K']:.4f}  r={best_logist['r']:.4f}  x0={best_logist['x0']:.4f}")
    print(f"    RMSE={best_logist['rmse']:.4f}  R²={best_logist['r2']:.4f}")

    # ── Class 4: Richards/generalised logistic  f = A/(1+exp(-r*(x-lam)))^(1/nu)
    # (extra shape parameter nu)
    from scipy.optimize import minimize
    def richards_loss(p):
        A, r, lam, nu = p
        nu = max(nu, 0.01)
        try:
            y_pred = A / (1 + np.exp(-r*(xs-lam)))**(1/nu)
            return float(np.mean((ys - y_pred)**2))
        except:
            return 1e9
    try:
        K0 = np.max(ys)
        res = minimize(richards_loss, [K0, 0.3, xs[len(xs)//2], 1.0],
                       method='Nelder-Mead', options={'maxiter':5000,'xatol':1e-5})
        A_r, r_r, lam_r, nu_r = res.x
        yp_rich = A_r / (1 + np.exp(-r_r*(xs-lam_r)))**(1/max(nu_r,0.01))
        rm_rich = rmse(ys, yp_rich); r2_rich = r2(ys, yp_rich)
        print(f"\n  Richards (generalised logistic):")
        print(f"    A={A_r:.4f}  r={r_r:.4f}  lam={lam_r:.4f}  nu={nu_r:.4f}")
        print(f"    RMSE={rm_rich:.4f}  R²={r2_rich:.4f}")
    except Exception as e:
        print(f"\n  Richards: failed ({e})")
        rm_rich, r2_rich = np.inf, -np.inf

    # ── Summary table
    print(f"\n  {'Model':<35} {'RMSE':>8}  {'R²':>8}")
    print(f"  {'─'*53}")
    models = [
        ("Gompertz (parametric)", grmse, gr2),
        ("Gompertz log-linear (ODE-derived)", best_c['rmse'], best_c['r2']),
        ("Logistic (linear class)", best_logist['rmse'], best_logist['r2']),
        ("Richards (generalised)", rm_rich, r2_rich),
        ("Linear ODE (oscillatory, WRONG)", r1, rr1),
    ]
    for name, err, rr in sorted(models, key=lambda x: x[1]):
        marker = " ✓" if rr > 0.99 else (" ✗" if rr < 0.9 else "")
        print(f"  {name:<35} {err:>8.4f}  {rr:>8.4f}{marker}")

    return {
        'gompertz_rmse': grmse, 'gompertz_r2': gr2,
        'log_linear_rmse': best_c['rmse'], 'log_linear_r2': best_c['r2'],
        'log_linear_c': best_c['c'],
    }


if __name__ == '__main__':
    csv_path = "../pinheiroTech/curves-data/all_curves.csv"
    print("="*60)
    print("  STEP A: Constant Fitting from Solution Classes")
    print("="*60)

    all_results = {}
    for col in [2, 3, 4]:
        try:
            xs, ys = load_csv_column(csv_path, col_idx=col)
            result = evaluate_all_classes(xs, ys, f"Real data — column {col}")
            all_results[col] = result
        except Exception as e:
            print(f"  Column {col} failed: {e}")

    # Synthetic validation: does log-linear class recover exact Gompertz?
    print(f"\n\n{'='*60}")
    print("  SYNTHETIC VALIDATION")
    print("="*60)
    t = np.linspace(0.5, 40, 150)
    for A, mu, lam, name in [(1.0, 0.3, 5.0, "Gompertz A=1 μ=0.3 λ=5"),
                              (1.2, 0.5, 8.0, "Gompertz A=1.2 μ=0.5 λ=8"),
                              (0.8, 0.2, 3.0, "Gompertz A=0.8 μ=0.2 λ=3")]:
        ys_syn = gompertz(t, A, mu, lam)
        r = evaluate_all_classes(t, ys_syn, name)

    print("\nStep A complete.")
