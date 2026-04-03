"""
Step D: ODE Discovery for aHPM (adjusted Heterogeneous Population Model)
=========================================================================
The aHPM describes a heterogeneous bacterial population with two subpopulations:
  - u1: lag subpopulation (not yet growing, exits lag at rate δ)
  - u2: growing subpopulation

ODE system:
  du1/dt = -u1 · δ
  du2/dt =  u1 · δ  +  gr · u2 · (1 - ((u1+u2)/Nmax)^shape)

Observable: X(t) = u1(t) + u2(t)

QUESTION: What is the effective ODE for the OBSERVABLE X(t) alone?
Can we discover a compact formula from X(t) data without knowing the
underlying 2-population structure?

The ODE for X = u1 + u2:
  X' = du1/dt + du2/dt
     = -u1·δ + u1·δ + gr·u2·(1 - (X/Nmax)^shape)
     = gr · u2 · (1 - (X/Nmax)^shape)

But u2 = X - u1, and u1(t) = u1(0)·exp(-δ·t) decays exponentially.
So:
  X'(t) = gr · (X - u1(0)·exp(-δ·t)) · (1 - (X/Nmax)^shape)

This is NON-AUTONOMOUS (depends on t explicitly through the exp(-δ·t) term).
It reduces to standard Richards/logistic only when u1(0) ≈ 0 (no lag population).

Goal: discover this structure from data using ODE inversion.
"""

import numpy as np
from scipy.integrate import solve_ivp
from scipy.interpolate import UnivariateSpline
from scipy.ndimage import gaussian_filter1d
from scipy.optimize import curve_fit, minimize
import warnings
warnings.filterwarnings('ignore')

# ── helpers ──────────────────────────────────────────────────────────

def rmse(y, yhat):
    return float(np.sqrt(np.mean((np.array(y) - np.array(yhat))**2)))

def r2(y, yhat):
    y, yhat = np.array(y), np.array(yhat)
    ss_res = np.sum((y - yhat)**2)
    ss_tot = np.sum((y - np.mean(y))**2)
    return float(1 - ss_res / ss_tot) if ss_tot > 0 else 0.0

def smooth_deriv(xs, ys, sigma_px=6, order=1):
    n = len(xs)
    spl = UnivariateSpline(xs, ys, k=5, s=n * (np.std(ys)*0.02)**2)
    xu = np.linspace(xs[0], xs[-1], max(n, 300))
    yu = spl(xu)
    dx = xu[1] - xu[0]
    edge = min(int(np.ceil(3*sigma_px)), (len(xu)-20)//2)
    d = gaussian_filter1d(yu, sigma_px, order=order, mode='nearest') / dx**order
    return xu[edge:-edge], d[edge:-edge], spl(xu[edge:-edge])


# ══════════════════════════════════════════════════════════════════════
# D1. Generate aHPM data
# ══════════════════════════════════════════════════════════════════════

def ahpm_ode(t, state, params):
    u1, u2 = state
    gr, delta, N_max, shape = params
    total = u1 + u2
    du1 = -u1 * delta
    du2 = u1 * delta + gr * u2 * (1 - max(0, total / N_max)**shape)
    return [du1, du2]

def generate_ahpm_data(gr=0.35, delta=0.20, N_max=1.0, shape=1.5,
                       u1_0=0.10, u2_0=0.005,
                       t_end=40.0, n_points=100, noise_std=0.005, seed=0):
    """
    Generate aHPM observable data X(t) = u1(t) + u2(t).

    Key parameters:
      gr    : intrinsic growth rate of the growing population
      delta : exit-lag rate (how fast lag bacteria become growing bacteria)
      N_max : carrying capacity
      shape : Richards shape (1=logistic, larger=more Gompertz-like)
      u1_0  : initial lag population (larger → more pronounced shoulder)
    """
    params = [gr, delta, N_max, shape]
    y0 = [u1_0, u2_0]
    sol = solve_ivp(ahpm_ode, [0, t_end], y0, args=(params,),
                    dense_output=True, rtol=1e-9, atol=1e-11)

    rng = np.random.default_rng(seed)
    ts = np.linspace(0, t_end, n_points)
    state = sol.sol(ts)
    X = state[0] + state[1]           # total observable
    u1 = state[0]                     # lag subpopulation (hidden)
    u2 = state[1]                     # growing subpopulation (hidden)
    X_noisy = X + rng.normal(0, noise_std, n_points)
    X_noisy = np.clip(X_noisy, 1e-4, None)
    return ts, X_noisy, X, u1, u2


# ══════════════════════════════════════════════════════════════════════
# D2. Fit standard growth models as baselines
# ══════════════════════════════════════════════════════════════════════

def gompertz(t, A, mu, lam):
    return A * np.exp(-np.exp((mu * np.e / A) * (lam - t) + 1))

def richards(t, A, r, lam, nu):
    nu = max(nu, 0.01)
    return A / (1 + np.exp(-r * (t - lam)))**(1/nu)

def fit_baselines(ts, ys):
    results = {}
    K = np.max(ys)

    # Gompertz
    try:
        p, _ = curve_fit(gompertz, ts, ys, p0=[K, 0.3, ts[len(ts)//3]],
                         bounds=([K*0.3,0,-ts[-1]], [K*3,5,ts[-1]]), maxfev=10000)
        yp = gompertz(ts, *p)
        results['gompertz'] = {'params': p, 'rmse': rmse(ys,yp), 'r2': r2(ys,yp)}
    except: pass

    # Richards (generalised logistic, 4 params)
    try:
        p, _ = curve_fit(richards, ts, ys, p0=[K, 0.3, ts[len(ts)//2], 1.0],
                         bounds=([K*0.3,0,0,0.01], [K*3,5,ts[-1],10]), maxfev=10000)
        yp = richards(ts, *p)
        results['richards'] = {'params': p, 'rmse': rmse(ys,yp), 'r2': r2(ys,yp)}
    except: pass

    return results


# ══════════════════════════════════════════════════════════════════════
# D3. Fit the theoretical aHPM solution directly
# ══════════════════════════════════════════════════════════════════════

def fit_ahpm_direct(ts, ys):
    """
    Fit the aHPM model directly: numerically integrate the ODE and
    fit [gr, delta, N_max, shape, u1_0, u2_0] to minimise MSE.

    This is the 'oracle' fit that knows the model structure.
    """
    def model_X(ts, gr, delta, N_max, shape, u1_0, u2_0):
        try:
            sol = solve_ivp(ahpm_ode, [ts[0], ts[-1]], [u1_0, u2_0],
                            args=([gr, delta, N_max, shape],),
                            dense_output=True, rtol=1e-8, atol=1e-10)
            state = sol.sol(ts)
            return state[0] + state[1]
        except:
            return np.full_like(ts, 1e6)

    K = np.max(ys)
    try:
        p0 = [0.35, 0.20, K*1.1, 1.5, ys[0]*0.5, ys[0]*0.1]
        bounds_lo = [0.01, 0.01, K*0.5, 0.1, 1e-4, 1e-5]
        bounds_hi = [5.0,  2.0,  K*3,   5.0, K*0.5, K*0.1]
        popt, _ = curve_fit(model_X, ts, ys, p0=p0,
                            bounds=(bounds_lo, bounds_hi), maxfev=5000)
        yp = model_X(ts, *popt)
        return {'params': popt, 'rmse': rmse(ys,yp), 'r2': r2(ys,yp)}
    except Exception as e:
        return {'error': str(e)}


# ══════════════════════════════════════════════════════════════════════
# D4. Discover the non-autonomous ODE structure:
#     X'(t) = gr · (X - u1_0·exp(-δ·t)) · (1 - (X/K)^shape)
#
# The key insight: X' depends on t EXPLICITLY via the lag term.
# We can detect this by fitting X'/(X · (1-(X/K)^shape)) ~ a + b·exp(-δ·t)
# If b ≠ 0, the data is NOT from a pure autonomous growth model.
# ══════════════════════════════════════════════════════════════════════

def detect_lag_correction(ts, ys, label=""):
    """
    Test whether X satisfies an autonomous ODE or a non-autonomous one
    with an explicit exponential lag correction.

    Model A (autonomous, Richards): X' = gr · X · (1 - (X/K)^s)
    Model B (non-autonomous, aHPM): X' = gr · (X - L·exp(-δ·t)) · (1 - (X/K)^s)

    Rewrite Model B:
      X' / (1 - (X/K)^s) = gr · X - gr·L·exp(-δ·t)
      = gr·X  - (gr·L) · exp(-δ·t)

    So: fit  φ(t) ≡ X' / (1-(X/K_est)^s_est)  as  a·X + b·exp(-δ·t)
    If b is significant → non-autonomous lag correction detected.
    """
    print(f"\n  D4 — Non-autonomous lag detection  [{label}]")

    # Step 1: estimate K and shape from Richards fit
    K_est = np.max(ys) * 1.05
    s_est = 1.5   # Richards shape, will be refined

    xu, xp, Xfit = smooth_deriv(ts, ys, sigma_px=5, order=1)

    # Step 2: compute phi = X' / (1 - (X/K)^s) — the "corrected rate"
    phi = xp / (1 - np.clip((Xfit / K_est)**s_est, 0, 0.999))

    # Step 3a: fit phi = a·X (autonomous hypothesis)
    A_auto = np.column_stack([Xfit])
    c_auto, _, _, _ = np.linalg.lstsq(A_auto, phi, rcond=None)
    phi_auto = c_auto[0] * Xfit
    res_auto = rmse(phi, phi_auto) / (np.std(phi) + 1e-10)
    print(f"    Autonomous model:     φ = {c_auto[0]:.4f}·X    residual = {res_auto:.4f}")

    # Step 3b: fit phi = a·X + b·exp(-δ·t) for multiple δ values
    best = {'res': np.inf}
    for delta_scan in np.linspace(0.01, 1.5, 50):
        A_lag = np.column_stack([Xfit, np.exp(-delta_scan * xu)])
        c_lag, _, _, _ = np.linalg.lstsq(A_lag, phi, rcond=None)
        phi_lag = A_lag @ c_lag
        res_lag = rmse(phi, phi_lag) / (np.std(phi) + 1e-10)
        if res_lag < best['res']:
            best = {'res': res_lag, 'delta': delta_scan,
                    'gr': c_lag[0], 'lag_term': c_lag[1]}

    print(f"    Non-autonomous model: φ = {best['gr']:.4f}·X + {best['lag_term']:.4f}·exp(-{best['delta']:.3f}·t)")
    print(f"                         residual = {best['res']:.4f}")

    improvement = res_auto - best['res']
    print(f"    Residual improvement from lag correction: {improvement:.4f}")

    if improvement > 0.05 and best['lag_term'] < -0.001:
        print(f"\n  → NON-AUTONOMOUS STRUCTURE DETECTED")
        print(f"    Effective ODE: X' = {best['gr']:.4f}·(X - {abs(best['lag_term'])/best['gr']:.4f}·exp(-{best['delta']:.3f}·t))·(1-(X/K)^s)")
        print(f"    Interpretation: lag population L₀ = {abs(best['lag_term'])/best['gr']:.4f}")
        print(f"                    exit-lag rate δ = {best['delta']:.3f}")
        return True, best
    else:
        print(f"  → Data consistent with autonomous ODE (no significant lag correction)")
        return False, best


# ══════════════════════════════════════════════════════════════════════
# D5. Compact formula search specifically for aHPM-shaped curves
# ══════════════════════════════════════════════════════════════════════

def ahpm_compact_search(ts, ys, label=""):
    """
    The theoretical aHPM solution (when shape=1, i.e. logistic growing pop):
      X(t) = X∞ - (X∞ - X_Richards) · correction(t)
    where the correction comes from the decaying lag population.

    More precisely: since u1(t) = u1_0·exp(-δ·t), the total X = u1 + u2
    has an initial "shoulder" compared to pure Richards growth.

    We search for compact formulas of the form:
      X(t) ≈ Richards(t) + c · exp(-δ·t)   (lag correction as additive term)

    This is a linearisation of the aHPM around the Richards baseline.
    """
    print(f"\n  D5 — Compact aHPM formula search  [{label}]")

    K = np.max(ys)
    results = {}

    # Form A: Richards + exponential lag correction
    # X(t) = A/(1+exp(-r(t-lam)))^(1/nu) + c·exp(-delta·t)
    def ahpm_linear(t, A, r, lam, nu, c, delta):
        nu = max(nu, 0.01)
        rich = A / (1 + np.exp(-r * (t - lam)))**(1/nu)
        return rich + c * np.exp(-delta * t)

    try:
        p0 = [K, 0.3, ts[len(ts)//2], 1.0, ys[0]*0.3, 0.2]
        popt, _ = curve_fit(ahpm_linear, ts, ys, p0=p0,
                            bounds=([K*0.3, 0, 0, 0.01, -K, 0.01],
                                    [K*3,   5, ts[-1], 10, K, 3]),
                            maxfev=10000)
        yp = ahpm_linear(ts, *popt)
        results['rich_lag'] = {'params': popt, 'rmse': rmse(ys,yp), 'r2': r2(ys,yp)}
        A, r, lam, nu, c, delta = popt
        print(f"    Richards + lag: A={A:.4f} r={r:.4f} λ={lam:.4f} ν={nu:.3f}")
        print(f"                    c={c:.4f} δ={delta:.4f}")
        print(f"                    RMSE={results['rich_lag']['rmse']:.4f}  R²={results['rich_lag']['r2']:.4f}")
        if c > 0:
            print(f"    c>0: initial lag population adds to OD before dying off")
            print(f"    Biological form: X = Richards_growth + u1_0·exp(-δ·t)")
        else:
            print(f"    c<0: initial lag OD is subtracted (strain dilution during lag)")
    except Exception as e:
        print(f"    Richards + lag: failed ({e})")

    # Form B: Gompertz + exponential lag (if Richards ν≈1)
    def ahpm_gompertz(t, A, mu, lam, c, delta):
        gomp = A * np.exp(-np.exp((mu * np.e / A) * (lam - t) + 1))
        return gomp + c * np.exp(-delta * t)

    try:
        p0 = [K, 0.3, ts[len(ts)//3], ys[0]*0.3, 0.2]
        popt2, _ = curve_fit(ahpm_gompertz, ts, ys, p0=p0,
                             bounds=([K*0.3, 0, -ts[-1], -K, 0.01],
                                     [K*3,   5,  ts[-1],  K, 3]),
                             maxfev=10000)
        yp2 = ahpm_gompertz(ts, *popt2)
        results['gomp_lag'] = {'params': popt2, 'rmse': rmse(ys,yp2), 'r2': r2(ys,yp2)}
        A, mu, lam, c, delta = popt2
        print(f"\n    Gompertz + lag:  A={A:.4f} μ={mu:.4f} λ={lam:.4f}")
        print(f"                     c={c:.4f} δ={delta:.4f}")
        print(f"                     RMSE={results['gomp_lag']['rmse']:.4f}  R²={results['gomp_lag']['r2']:.4f}")
    except Exception as e:
        print(f"    Gompertz + lag: failed ({e})")

    # Form C: Pure autonomous aHPM ODE in compact form
    # Since X'=gr·u2·(1-(X/K)^s) and u2=X-u1=X-u1_0·exp(-δt):
    # X' = gr·(X - u1_0·exp(-δt))·(1-(X/K)^s)
    # This is a compact 2-parameter extension of Richards.
    def ahpm_ode_compact(t, X, gr, delta, K, shape, u1_0):
        lag = u1_0 * np.exp(-delta * t)
        u2 = max(X - lag, 0)
        return gr * u2 * (1 - min((X / K)**shape, 1))

    # Fit via scipy minimization (can't use curve_fit directly since it's an ODE)
    def ahpm_loss(params):
        gr, delta, K, shape, u1_0 = params
        if gr <= 0 or delta <= 0 or K <= 0 or shape <= 0 or u1_0 < 0:
            return 1e9
        u2_0 = max(ys[0] - u1_0, 1e-5)
        try:
            sol = solve_ivp(lambda t, X: [ahpm_ode_compact(t, X[0], gr, delta, K, shape, u1_0)],
                            [ts[0], ts[-1]], [ys[0]],
                            dense_output=True, rtol=1e-6, atol=1e-8)
            X_pred = sol.sol(ts)[0]
            return float(np.mean((ys - X_pred)**2))
        except:
            return 1e9

    try:
        from scipy.optimize import minimize
        K_est = np.max(ys) * 1.05
        res = minimize(ahpm_loss, [0.35, 0.20, K_est, 1.5, ys[0]*0.5],
                       method='Nelder-Mead',
                       options={'maxiter': 5000, 'xatol': 1e-5, 'fatol': 1e-8})
        gr, delta, K, shape, u1_0 = res.x
        u2_0 = max(ys[0] - u1_0, 1e-5)
        sol = solve_ivp(lambda t, X: [ahpm_ode_compact(t, X[0], gr, delta, K, shape, u1_0)],
                        [ts[0], ts[-1]], [ys[0]],
                        dense_output=True, rtol=1e-6, atol=1e-8)
        yp3 = sol.sol(ts)[0]
        r2_val = r2(ys, yp3)
        rmse_val = rmse(ys, yp3)
        results['ahpm_compact'] = {'params': res.x, 'rmse': rmse_val, 'r2': r2_val}
        print(f"\n    Compact aHPM ODE: X' = gr·(X - L·exp(-δ·t))·(1-(X/K)^s)")
        print(f"      gr={gr:.4f}  δ={delta:.4f}  K={K:.4f}  s={shape:.3f}  L={u1_0:.4f}")
        print(f"      RMSE={rmse_val:.4f}  R²={r2_val:.4f}")
        print(f"\n    This is the KEY aHPM formula:")
        print(f"      X'(t) = {gr:.3f} · (X - {u1_0:.3f}·exp(-{delta:.3f}·t)) · (1 - (X/{K:.3f})^{shape:.2f})")
        print(f"\n    Biological meaning:")
        print(f"      gr={gr:.3f}: growth rate of actively growing cells")
        print(f"      δ={delta:.3f}: exit-lag rate (1/δ ≈ {1/delta:.1f}h mean lag time)")
        print(f"      K={K:.3f}: carrying capacity")
        print(f"      s={shape:.2f}: Richards shape (s=1 → logistic, s→∞ → Gompertz)")
        print(f"      L={u1_0:.3f}: initial lag population fraction")
    except Exception as e:
        print(f"    Compact aHPM ODE: failed ({e})")

    # Summary
    if results:
        print(f"\n  Summary:")
        for name, res in sorted(results.items(), key=lambda x: x[1].get('rmse', 1)):
            r2v = res.get('r2', float('nan'))
            print(f"    {name:<20} RMSE={res.get('rmse',1):.4f}  R²={r2v:.4f}")

    return results


# ══════════════════════════════════════════════════════════════════════
# D6. aHPM vs Gompertz: what makes them different?
# ══════════════════════════════════════════════════════════════════════

def compare_ahpm_gompertz(ts, X_obs, X_true, u1, u2, label=""):
    """
    Show analytically why aHPM produces a different shape than Gompertz:
    - The 'shoulder' at early times (lag population adds to OD)
    - The inflection position shifts
    - The derivative peak is broader/later
    """
    print(f"\n  D6 — aHPM vs Gompertz comparison  [{label}]")

    # Compute derivative fingerprint
    xu, xp, Xfit = smooth_deriv(ts, X_obs, sigma_px=5, order=1)
    peak_t = xu[np.argmax(xp)]
    peak_val = np.max(xp)
    peak_X = Xfit[np.argmax(xp)]

    X_min, X_max = Xfit.min(), Xfit.max()
    inflection_rel = (peak_X - X_min) / (X_max - X_min)

    print(f"    Peak growth rate: {peak_val:.4f} at t={peak_t:.2f}")
    print(f"    X at peak growth: {peak_X:.4f}")
    print(f"    Inflection relative height: {inflection_rel:.3f}")
    print(f"    (Gompertz=0.370, Logistic=0.500)")

    # At peak: u1/X ratio
    spl_u1 = UnivariateSpline(ts, u1, k=3, s=len(ts)*0.01)
    spl_X  = UnivariateSpline(ts, X_true, k=3, s=len(ts)*0.01)
    u1_at_peak = max(0, spl_u1(peak_t))
    X_at_peak  = spl_X(peak_t)
    u1_fraction = u1_at_peak / (X_at_peak + 1e-10)
    print(f"    Lag fraction at peak: {u1_fraction:.3f} (u1/X = {u1_at_peak:.4f}/{X_at_peak:.4f})")

    # Half-width of derivative peak
    half_max = peak_val * 0.5
    above_half = xu[xp >= half_max]
    if len(above_half) > 1:
        width = above_half[-1] - above_half[0]
        print(f"    Peak half-width: {width:.2f}h (broader → more Gompertz-like, narrower → logistic-like)")

    if inflection_rel < 0.37:
        print(f"\n  → Inflection BELOW 37%: sub-Gompertz (extreme shoulder from lag pop)")
    elif inflection_rel < 0.40:
        print(f"\n  → Inflection near 37%: Gompertz-like (mild shoulder)")
    elif inflection_rel < 0.50:
        print(f"\n  → Inflection between 37-50%: between Gompertz and Logistic")
    else:
        print(f"\n  → Inflection above 50%: super-logistic")

    return {'inflection': inflection_rel, 'peak_t': peak_t, 'peak_val': peak_val}


# ══════════════════════════════════════════════════════════════════════
# Main
# ══════════════════════════════════════════════════════════════════════

if __name__ == '__main__':
    print("="*65)
    print("  STEP D: ODE Discovery for aHPM Bacterial Growth")
    print("="*65)

    # ── Case 1: Strong lag (prominent shoulder) ───────────────────
    print("\n" + "█"*65)
    print("  Case 1: Strong lag  (u1_0=0.15, δ=0.15 → slow exit)")
    print("█"*65)

    ts1, X_obs1, X_true1, u1_1, u2_1 = generate_ahpm_data(
        gr=0.35, delta=0.15, N_max=1.0, shape=1.5,
        u1_0=0.15, u2_0=0.005, t_end=40.0, n_points=100)

    print(f"\n  Generated {len(ts1)} points, X ∈ [{X_obs1.min():.4f}, {X_obs1.max():.4f}]")
    print(f"  True params: gr=0.35  δ=0.15  Nmax=1.0  shape=1.5  u1_0=0.15")

    print("\n  D2 — Baseline parametric fits:")
    bl1 = fit_baselines(ts1, X_obs1)
    for name, res in sorted(bl1.items(), key=lambda x: x[1]['rmse']):
        print(f"    {name:<12} RMSE={res['rmse']:.4f}  R²={res['r2']:.4f}")

    compare_ahpm_gompertz(ts1, X_obs1, X_true1, u1_1, u2_1, "strong lag")
    is_nonauto, lag_det = detect_lag_correction(ts1, X_obs1, "strong lag")
    ahpm_compact_search(ts1, X_obs1, "strong lag")

    # ── Case 2: Mild lag ──────────────────────────────────────────
    print("\n\n" + "█"*65)
    print("  Case 2: Mild lag  (u1_0=0.03, δ=0.40 → fast exit)")
    print("█"*65)

    ts2, X_obs2, X_true2, u1_2, u2_2 = generate_ahpm_data(
        gr=0.35, delta=0.40, N_max=1.0, shape=1.5,
        u1_0=0.03, u2_0=0.005, t_end=40.0, n_points=100)

    print(f"\n  Generated {len(ts2)} points, X ∈ [{X_obs2.min():.4f}, {X_obs2.max():.4f}]")

    print("\n  D2 — Baseline parametric fits:")
    bl2 = fit_baselines(ts2, X_obs2)
    for name, res in sorted(bl2.items(), key=lambda x: x[1]['rmse']):
        print(f"    {name:<12} RMSE={res['rmse']:.4f}  R²={res['r2']:.4f}")

    compare_ahpm_gompertz(ts2, X_obs2, X_true2, u1_2, u2_2, "mild lag")
    is_nonauto2, lag_det2 = detect_lag_correction(ts2, X_obs2, "mild lag")

    # ── Synthesis ─────────────────────────────────────────────────
    print("\n\n" + "="*65)
    print("  SYNTHESIS: aHPM Discovery Results")
    print("="*65)
    print("""
  Key findings:

  1. THEORETICAL DERIVATION
     The aHPM observable X(t) = u1(t) + u2(t) satisfies the
     non-autonomous ODE:

       X'(t) = gr · (X - L₀·exp(-δ·t)) · (1 - (X/K)^s)

     where L₀ = u1(0) is the initial lag population.
     This reduces to Richards when L₀=0 (no lag population).
     The lag correction L₀·exp(-δ·t) is the key novel term.

  2. DETECTABILITY
     The lag correction can be detected by regressing the
     "corrected rate" φ(t) = X' / (1-(X/K)^s) against:
       - Autonomous model: φ = gr·X
       - Non-autonomous:   φ = gr·X - gr·L₀·exp(-δ·t)
     Significant improvement → lag structure present.

  3. COMPACT FORMULA (NEW MODEL)
     The compact non-autonomous ODE has 5 parameters:
       {gr, δ, K, s, L₀}
     vs Richards (4 params) vs aHPM-full (6 params).

     The FORMULA for X(t) (approximate, when δ >> gr):
       X(t) ≈ Richards(gr, K, s, t) + L₀·exp(-δ·t)·f(t)
     where f(t) is a correction factor from the lag coupling.

  4. BIOLOGICAL INTERPRETATION
     - L₀: what fraction of the inoculum is in lag phase
     - δ:  how quickly lag bacteria adapt (1/δ = mean lag time)
     - The formula quantifies "inoculum history effects"
     - Important for food safety (predicting time to dangerous OD)
""")

    print("Step D complete.")
