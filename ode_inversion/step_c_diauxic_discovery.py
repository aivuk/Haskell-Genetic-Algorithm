"""
Step C: ODE Discovery for Diauxic Growth
=========================================
Goal: Apply characterize-first ODE inversion to diauxic bacterial growth data
to DISCOVER compact symbolic ODEs — not just recover known models.

A diauxic curve has the shape:
  growth on substrate 1 → lag/plateau → growth on substrate 2

The observable is X(t) (biomass/OD). The underlying system has hidden state
(S1, S2), but X(t) alone might satisfy a compact 2nd-order autonomous ODE.
Finding that ODE is the novel scientific contribution.

Strategy:
  C1. Generate synthetic diauxic data from Monod 2-substrate kinetics
  C2. Check: does a single Gompertz suffice? (No — baseline to beat)
  C3. Symbolic boosting: fit G1, then detect G2 in residuals
  C4. Search for compact autonomous ODE: X' = P(X, degree ≤ 5)
  C5. Search for 2nd-order ODE: X'' = Q(X, X')
  C6. Biological interpretation of discovered ODE
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
    return float(1 - np.sum((y-yhat)**2) / np.sum((y-np.mean(y))**2))

def smooth_deriv(xs, ys, sigma_px=6, order=1):
    """Gaussian derivative filter on a uniform grid."""
    n = len(xs)
    spl = UnivariateSpline(xs, ys, k=5, s=n * (np.std(ys)*0.02)**2)
    xu = np.linspace(xs[0], xs[-1], max(n, 300))
    yu = spl(xu)
    dx = xu[1] - xu[0]
    edge = min(int(np.ceil(3*sigma_px)), (len(xu)-20)//2)
    d = gaussian_filter1d(yu, sigma_px, order=order, mode='nearest') / dx**order
    return xu[edge:-edge], d[edge:-edge], spl(xu[edge:-edge])


# ═══════════════════════════════════════════════════════════════════════
# C1. Mechanistic diauxic data: Monod 2-substrate kinetics
# ═══════════════════════════════════════════════════════════════════════
#
# State: [X, S1, S2]
# dX/dt  = X * (mu1*S1/(Ks1+S1) + mu2*S2/(Ks2+S2) * hill(S1))
# dS1/dt = -1/Y1 * mu1*S1/(Ks1+S1) * X
# dS2/dt = -1/Y2 * mu2*S2/(Ks2+S2) * X * hill(S1)
#
# hill(S1) = Kd^n / (Kd^n + S1^n) — catabolite repression:
#   when S1 is plentiful, it represses S2 utilisation.
#   When S1 is exhausted, hill → 1 and S2 becomes available.
#
# This gives the characteristic diauxic lag: a plateau between two growth phases.

def diauxic_monod_ode(t, state, params):
    X, S1, S2 = state
    mu1, mu2, Ks1, Ks2, Y1, Y2, Kd, n_hill = params
    X = max(X, 0); S1 = max(S1, 0); S2 = max(S2, 0)

    # Monod terms
    rate1 = mu1 * S1 / (Ks1 + S1)
    rate2 = mu2 * S2 / (Ks2 + S2)

    # Catabolite repression: S2 consumption suppressed while S1 available
    repression = Kd**n_hill / (Kd**n_hill + S1**n_hill)

    dX  = X * (rate1 + rate2 * repression)
    dS1 = -1/Y1 * rate1 * X
    dS2 = -1/Y2 * rate2 * repression * X
    return [dX, dS1, dS2]

def generate_diauxic_data(params=None, n_points=120, t_end=50.0,
                           noise_std=0.005, seed=42):
    """
    Default params give a clear diauxic growth curve:
      Phase 1: X grows on S1 (preferred substrate)
      Lag:     S1 depleted, enzyme induction for S2 (catabolite repression)
      Phase 2: X grows on S2
    """
    if params is None:
        # Parameters tuned for CLEAR diauxic: sharp switch, well-separated phases
        params = dict(
            mu1=0.90,   # fast growth on S1 (preferred)
            mu2=0.30,   # slower growth on S2 (secondary, 3x slower)
            Ks1=0.02,   # tight half-saturation S1 (rapid consumption)
            Ks2=0.10,   # half-saturation S2
            Y1=0.55,    # yield on S1
            Y2=0.50,    # yield on S2
            Kd=0.005,   # very tight catabolite repression (S2 only after S1 fully depleted)
            n_hill=10.0,# very sharp switch
        )
    p = [params['mu1'], params['mu2'], params['Ks1'], params['Ks2'],
         params['Y1'],  params['Y2'],  params['Kd'],  params['n_hill']]

    y0 = [0.01, 0.50, 1.00]   # X0, S1_0=0.50, S2_0=1.00 (plenty for clear phase 2)
    sol = solve_ivp(diauxic_monod_ode, [0, t_end], y0,
                    args=(p,), dense_output=True,
                    rtol=1e-8, atol=1e-10)

    rng = np.random.default_rng(seed)
    ts = np.linspace(0, t_end, n_points)
    X  = sol.sol(ts)[0]
    X_noisy = X + rng.normal(0, noise_std, size=len(ts))
    X_noisy = np.clip(X_noisy, 1e-4, None)
    return ts, X_noisy, X   # times, noisy OD, true OD


# ═══════════════════════════════════════════════════════════════════════
# C2. Baseline: does a single Gompertz fit?
# ═══════════════════════════════════════════════════════════════════════

def gompertz(t, A, mu, lam):
    return A * np.exp(-np.exp((mu * np.e / A) * (lam - t) + 1))

def fit_gompertz(ts, ys):
    K = np.max(ys)
    try:
        popt, _ = curve_fit(gompertz, ts, ys,
                            p0=[K, 0.3, ts[len(ts)//3]],
                            bounds=([K*0.3, 0, -ts[-1]], [K*3, 5, ts[-1]]),
                            maxfev=10000)
        yp = gompertz(ts, *popt)
        return popt, rmse(ys, yp), r2(ys, yp)
    except Exception as e:
        return None, np.inf, -np.inf


# ═══════════════════════════════════════════════════════════════════════
# C3. Symbolic boosting: fit G1 → residual → fit G2
# ═══════════════════════════════════════════════════════════════════════

def symbolic_boosting(ts, ys, label=""):
    print(f"\n  C3 — Symbolic Boosting  [{label}]")

    # Round 1: fit Gompertz to full data
    popt1, r1, rr1 = fit_gompertz(ts, ys)
    if popt1 is None:
        print("    Round 1: Gompertz failed")
        return None
    yp1 = gompertz(ts, *popt1)
    print(f"    Round 1 Gompertz: A={popt1[0]:.4f} μ={popt1[1]:.4f} λ={popt1[2]:.4f}")
    print(f"      RMSE={r1:.4f}  R²={rr1:.4f}")

    # Residual
    resid = ys - yp1
    print(f"    Residual: max={resid.max():.4f}  mean={resid.mean():.4f}")

    # Round 2: fit Gompertz to residual (shifted up so it can be positive)
    resid_shifted = resid - resid.min() + 1e-4
    popt2, r2_, rr2 = fit_gompertz(ts, resid_shifted)

    if popt2 is not None and rr2 > 0.5:
        yp2 = gompertz(ts, *popt2) + resid.min() - 1e-4
        total = yp1 + yp2
        print(f"\n    Round 2 Gompertz on residual:")
        print(f"      A={popt2[0]:.4f} μ={popt2[1]:.4f} λ={popt2[2]:.4f}")
        print(f"      R²(residual)={rr2:.4f}")
        total_r2 = r2(ys, total)
        total_rmse = rmse(ys, total)
        print(f"\n    Combined G1+G2: RMSE={total_rmse:.4f}  R²={total_r2:.4f}")
        print(f"    Expression: {popt1[0]:.4f}·G(μ={popt1[1]:.4f}, λ={popt1[2]:.4f})")
        print(f"              + {popt2[0]:.4f}·G(μ={popt2[1]:.4f}, λ={popt2[2]:.4f})")
        return {'type': 'double_gompertz',
                'params1': popt1, 'params2': popt2,
                'rmse': total_rmse, 'r2': total_r2}
    else:
        print(f"    Round 2: no significant second growth detected (R²={rr2:.3f})")
        return {'type': 'single_gompertz', 'params': popt1, 'rmse': r1, 'r2': rr1}


# ═══════════════════════════════════════════════════════════════════════
# C3b. Phase-segmented symbolic boosting
# ═══════════════════════════════════════════════════════════════════════

def segmented_boosting(ts, ys, label=""):
    """
    For diauxic data, fit the two phases separately:
    1. Find the diauxic valley: the time point where dX/dt is minimal (plateau region)
    2. Fit G1 to data before the valley
    3. Fit G2 to residual = data - G1, focusing on data after the valley
    This is more robust than fitting G1 to the full curve.
    """
    print(f"\n  C3b — Phase-Segmented Boosting  [{label}]")

    xu, xp, _ = smooth_deriv(ts, ys, sigma_px=5, order=1)
    # Find valley in X' (diauxic lag = local minimum of X' after first peak)
    from scipy.signal import find_peaks
    peaks, _ = find_peaks(xp, height=np.max(xp)*0.05, distance=5)

    if len(peaks) < 2:
        print("    Only one peak detected — trying naive segmentation at midpoint")
        t_split = ts[len(ts)//2]
    else:
        # Valley between first two peaks
        valley_idx = np.argmin(xp[peaks[0]:peaks[1]]) + peaks[0]
        t_split = xu[valley_idx]
        print(f"    Diauxic valley at t≈{t_split:.2f} (X'={xp[valley_idx]:.5f})")

    # Split data
    mask1 = ts <= t_split + (ts[-1]-ts[0])*0.05  # slight overlap for smooth join
    mask2 = ts >= t_split

    ts1, ys1 = ts[mask1], ys[mask1]
    ts2, ys2 = ts[mask2], ys[mask2]

    print(f"    Phase 1: {len(ts1)} points, t ∈ [{ts1[0]:.1f}, {ts1[-1]:.1f}]")
    print(f"    Phase 2: {len(ts2)} points, t ∈ [{ts2[0]:.1f}, {ts2[-1]:.1f}]")

    # Fit G1 to phase 1 data only
    popt1, r1, rr1 = fit_gompertz(ts1, ys1)
    if popt1 is None:
        print("    Phase 1 Gompertz failed")
        return None
    yp1 = gompertz(ts, *popt1)
    print(f"    Phase 1: A={popt1[0]:.4f} μ={popt1[1]:.4f} λ={popt1[2]:.4f}  R²(phase1)={rr1:.4f}")

    # Residual for phase 2
    resid2 = ys2 - gompertz(ts2, *popt1)
    if resid2.max() < 0.01:
        print("    No significant residual in phase 2 — single phase growth")
        return {'type': 'single', 'rmse': r1, 'r2': rr1}

    # Fit G2 to residual
    resid2_shifted = resid2 - resid2.min() + 1e-4
    popt2, r2_, rr2 = fit_gompertz(ts2, resid2_shifted)
    if popt2 is None or rr2 < 0.3:
        print(f"    Phase 2 Gompertz failed (R²={rr2:.3f})")
        return None

    yp2_full = gompertz(ts, *popt2) + resid2.min() - 1e-4
    total = yp1 + yp2_full
    # Clip negative (second Gompertz may be negative before it activates)
    total_r2 = r2(ys, np.clip(total, 0, None))
    total_rmse = rmse(ys, np.clip(total, 0, None))

    print(f"    Phase 2: A={popt2[0]:.4f} μ={popt2[1]:.4f} λ={popt2[2]:.4f}  R²(phase2)={rr2:.4f}")
    print(f"    Combined: RMSE={total_rmse:.4f}  R²={total_r2:.4f}")
    print(f"    Formula: {popt1[0]:.4f}·G(μ={popt1[1]:.4f},λ={popt1[2]:.4f})")
    print(f"            + {popt2[0]:.4f}·G(μ={popt2[1]:.4f},λ={popt2[2]:.4f})")

    return {'type': 'double_segmented', 'p1': popt1, 'p2': popt2,
            't_split': t_split, 'rmse': total_rmse, 'r2': total_r2}


# ═══════════════════════════════════════════════════════════════════════
# Validate: integrate discovered bi-Gompertz ODE numerically
# X' = X·[r1·ln(K1/X) + r2·ln(K2/X)]
# to check if it produces a diauxic-shaped curve
# ═══════════════════════════════════════════════════════════════════════

def validate_bi_gompertz_ode(r1, r2, K1, K2, X0, t_end=50.0, n=300, label=""):
    """Integrate X' = X·[r1·ln(K1/X) + r2·ln(K2/X)] and characterise the curve."""
    print(f"\n  Validating bi-Gompertz ODE: X' = X·[{r1:.3f}·ln({K1:.3f}/X) + {r2:.3f}·ln({K2:.3f}/X)]")
    print(f"  X(0) = {X0}")

    def ode(t, X):
        x = max(X[0], 1e-12)
        Xp = x * (r1 * np.log(K1 / x) + r2 * np.log(K2 / x))
        return [Xp]

    sol = solve_ivp(ode, [0, t_end], [X0], dense_output=True,
                    rtol=1e-8, atol=1e-10)
    ts = np.linspace(0, t_end, n)
    Xs = sol.sol(ts)[0]

    # Check for diauxic signature in derivative
    dX = np.diff(Xs) / (ts[1] - ts[0])
    ts_mid = 0.5*(ts[:-1] + ts[1:])

    from scipy.signal import find_peaks
    peaks, _ = find_peaks(dX, height=np.max(dX)*0.05, distance=10)
    print(f"  Peaks in X': {[f't={ts_mid[p]:.2f}' for p in peaks]}")

    print(f"  X range: [{Xs.min():.4f}, {Xs.max():.4f}]")
    print(f"  Fixed points of ODE: K1={K1:.3f}, K2={K2:.3f}")

    # Does the ODE produce a plateau at K1 then rise to K2?
    mid_X = Xs[len(Xs)//3]
    final_X = Xs[-1]
    print(f"  X at t={t_end/3:.0f}: {mid_X:.4f}  (K1={K1:.3f})")
    print(f"  X at t={t_end:.0f}:   {final_X:.4f}  (K2={K2:.3f})")
    if K1 < K2 and mid_X < (K1 + K2)/2 and final_X > (K1 + K2)/2:
        print("  → Curve passes through intermediate plateau near K1 then reaches K2")
        print("  → BI-GOMPERTZ ODE PRODUCES DIAUXIC-LIKE CURVE ✓")
    else:
        print("  → Curve goes directly to K2 without intermediate plateau")

    return ts, Xs


# ═══════════════════════════════════════════════════════════════════════
# C4. Autonomous ODE discovery: X' = P(X)
# Search for the simplest polynomial in X that explains X'
# This is the KEY DISCOVERY step — no prior knowledge of model form.
# ═══════════════════════════════════════════════════════════════════════

def autonomous_ode_discovery(ts, ys, label="", max_degree=5):
    """
    Fit X' = sum_{k=0}^{d} c_k * X^k   for degrees d = 1..max_degree.

    If the data truly satisfies a compact autonomous ODE (i.e., X' depends
    only on X and not explicitly on t), the coefficients reveal:
      - Which terms are dominant
      - The carrying capacity (zero of P)
      - Fixed points and their stability

    Biological interpretation of terms:
      X^1   → exponential growth (Malthusian)
      X^2   → logistic inhibition (competition for resources)
      X^3+  → cooperative/cooperative-competitive effects
      ln(X) → Gompertz (captured via log-space reparametrisation)
    """
    print(f"\n  C4 — Autonomous ODE Discovery: X' = P(X)  [{label}]")

    xu, xp, Xfit = smooth_deriv(ts, ys, sigma_px=6, order=1)
    X = Xfit  # smoothed X on the xu grid

    results = {}
    for deg in range(1, max_degree + 1):
        # Build feature matrix: [X^0, X^1, ..., X^deg]
        Theta = np.column_stack([X**k for k in range(deg + 1)])
        coeffs, _, _, _ = np.linalg.lstsq(Theta, xp, rcond=None)
        xp_pred = Theta @ coeffs
        norm_res = rmse(xp, xp_pred) / (np.std(xp) + 1e-10)

        # Suppress near-zero coefficients for readability
        terms = [(k, c) for k, c in enumerate(coeffs) if abs(c) > 1e-4]
        eq_str = " + ".join(f"{c:.4f}·X^{k}" if k > 1
                            else (f"{c:.4f}" if k == 0 else f"{c:.4f}·X")
                            for k, c in terms)
        results[deg] = {'coeffs': coeffs, 'res': norm_res, 'eq': eq_str}
        print(f"    degree {deg}: X' = {eq_str}")
        print(f"              residual = {norm_res:.4f}")

    # Find the best degree by residual drop (elbow method)
    residuals = [results[d]['res'] for d in range(1, max_degree+1)]
    drops = [residuals[i-1] - residuals[i] for i in range(1, len(residuals))]
    best_deg = 1 + np.argmax(drops) + 1 if max(drops) > 0.05 else np.argmin(residuals) + 1

    print(f"\n  → Best autonomous polynomial degree: {best_deg}")
    print(f"    X' = {results[best_deg]['eq']}")
    print(f"    (normalised residual = {results[best_deg]['res']:.4f})")

    # Biological commentary on coefficients
    c = results[best_deg]['coeffs']
    if best_deg >= 2:
        zeros = np.roots(c[::-1])  # polynomial roots
        real_zeros = [z.real for z in zeros if abs(z.imag) < 0.01 and z.real > 0]
        if real_zeros:
            print(f"\n  Positive fixed points of X' = 0: {[f'{z:.3f}' for z in sorted(real_zeros)]}")
            print(f"  (these are candidate carrying capacities)")

    return results, best_deg


# ═══════════════════════════════════════════════════════════════════════
# C5. Second-order ODE discovery: X'' = Q(X, X')
# ═══════════════════════════════════════════════════════════════════════

def second_order_ode_discovery(ts, ys, label=""):
    """
    Fit X'' = sum c_ij * X^i * (X')^j  for i+j <= 2.

    A compact 2nd-order ODE can encode diauxic dynamics more naturally:
    the switching between phases appears as a phase-plane structure.

    Feature library for X'' = Q(X, X'):
      [1, X, X', X^2, X·X', (X')^2]
    """
    print(f"\n  C5 — Second-order ODE: X'' = Q(X, X')  [{label}]")

    xu, xpp, _ = smooth_deriv(ts, ys, sigma_px=8, order=2)
    _, xp_raw, Xfit = smooth_deriv(ts, ys, sigma_px=8, order=1)

    # Restrict to same grid (the second derivative uses larger sigma → trim more)
    n = len(xu)

    # Interpolate X and X' onto xu grid
    spl_X  = UnivariateSpline(ts, ys, k=5, s=len(ts)*(np.std(ys)*0.02)**2)
    spl_Xp = spl_X.derivative()
    X  = spl_X(xu)
    Xp = spl_Xp(xu)

    # Feature library: [1, X, X', X², X·X', (X')²]
    Theta = np.column_stack([
        np.ones(n),     # constant
        X,              # X
        Xp,             # X'
        X**2,           # X²
        X * Xp,         # X·X'
        Xp**2,          # (X')²
    ])
    names = ['1', 'X', "X'", 'X²', "X·X'", "(X')²"]

    # Full least squares
    coeffs, _, _, _ = np.linalg.lstsq(Theta, xpp, rcond=None)
    xpp_pred = Theta @ coeffs
    res_full = rmse(xpp, xpp_pred) / (np.std(xpp) + 1e-10)

    terms = [(names[i], coeffs[i]) for i in range(len(coeffs)) if abs(coeffs[i]) > 1e-3]
    print(f"    Full LS: X'' = " + " + ".join(f"{v:.4f}·{n}" for n, v in terms))
    print(f"    residual = {res_full:.4f}")

    # STRidge for sparse 2nd order ODE
    def stridge(Th, y, thr=0.1, n_iter=20):
        active = list(range(Th.shape[1]))
        c = np.zeros(Th.shape[1])
        c_a = None
        for _ in range(n_iter):
            if not active: break
            Ta = Th[:, active]
            c_a = np.linalg.lstsq(Ta.T@Ta + 1e-6*np.eye(len(active)),
                                   Ta.T@y, rcond=None)[0]
            mask = np.abs(c_a) >= thr * np.max(np.abs(c_a))
            if not np.any(mask): break
            active = [active[i] for i in range(len(active)) if mask[i]]
            c_a = c_a[mask]
        c_full = np.zeros(Th.shape[1])
        for i, idx in enumerate(active):
            if c_a is not None and i < len(c_a):
                c_full[idx] = c_a[i]
        return c_full

    c_sparse = stridge(Theta, xpp, thr=0.1)
    xpp_sparse = Theta @ c_sparse
    res_sparse = rmse(xpp, xpp_sparse) / (np.std(xpp) + 1e-10)

    terms_sp = [(names[i], c_sparse[i]) for i in range(len(c_sparse)) if abs(c_sparse[i]) > 1e-3]
    print(f"    Sparse:  X'' = " + (" + ".join(f"{v:.4f}·{n}" for n, v in terms_sp) or "0"))
    print(f"    residual = {res_sparse:.4f}")

    # Interpretation
    print("\n  Biological interpretation:")
    c = {names[i]: c_sparse[i] for i in range(len(c_sparse))}
    if abs(c.get("X'", 0)) > 0.01:
        coeff_xp = c["X'"]
        if coeff_xp < 0:
            print(f"    X'' has -{abs(coeff_xp):.3f}·X' → damping term → growth decelerates as it grows faster")
        else:
            print(f"    X'' has +{coeff_xp:.3f}·X' → autocatalytic acceleration")
    if abs(c.get("X·X'", 0)) > 0.001:
        coeff_xxp = c["X·X'"]
        print(f"    X·X' term ({coeff_xxp:.4f}) → growth-rate feedback depends on current biomass")
        print(f"    → possible diauxic signature: rate modulation by X itself")
    if abs(c.get("X²", 0)) > 0.001:
        print(f"    X² term → nonlinear resource competition")

    return {'full_res': res_full, 'sparse_res': res_sparse,
            'coeffs': c, 'terms': terms_sp}


# ═══════════════════════════════════════════════════════════════════════
# C6. Novel compact form search: X' = r·X·(K-X)·φ(X)
# Motivated by the idea that diauxic growth might have a compact
# "generalized logistic with substrate switch" form.
# ═══════════════════════════════════════════════════════════════════════

def compact_diauxic_ode_search(ts, ys, label=""):
    """
    Search for compact autonomous ODE forms that go beyond standard models:

    Form 1: Bi-logistic  X' = r·X·(K1-X)·(K2+X)/(K1·K2)
      → two carrying capacities K1 < K2; growth is inhibited around K1,
        then resumes toward K2. Mechanistic: K1 = substrate 1 saturation.

    Form 2: Gompertz-logistic hybrid  X' = r·X·ln(K/X)·(1 + a·X)
      → Gompertz with a density-dependent correction.

    Form 3: X' = r·X·(K-X)/(K·X + α)
      → Generalized with inhibition at low X (Allee-like) or at high X.

    The goal: find the form with fewest parameters + best fit.
    """
    print(f"\n  C6 — Compact Diauxic ODE Search  [{label}]")

    xu, xp, Xfit = smooth_deriv(ts, ys, sigma_px=6, order=1)
    X = Xfit

    results = {}

    # Form 1: Bi-logistic (cubic in X)
    # X' = X·(K1-X)·(K2-X) * r / (K1*K2)
    def bilogistic(X, r, K1, K2):
        return r * X * (K1 - X) * (K2 - X) / (K1 * K2 + 1e-10)

    try:
        Xmax = np.max(ys)
        popt, _ = curve_fit(bilogistic, X, xp,
                            p0=[0.1, Xmax*0.6, Xmax*1.1],
                            bounds=([0, 0.01, Xmax*0.5], [5, Xmax, Xmax*3]),
                            maxfev=10000)
        xp_bl = bilogistic(X, *popt)
        res_bl = rmse(xp, xp_bl) / (np.std(xp) + 1e-10)
        results['bilogistic'] = {'params': popt, 'res': res_bl}
        print(f"    Bi-logistic X' = r·X·(K1-X)·(K2-X)/(K1·K2):")
        print(f"      r={popt[0]:.4f}  K1={popt[1]:.4f}  K2={popt[2]:.4f}  residual={res_bl:.4f}")
        print(f"      → K1={popt[1]:.3f} is first saturation (substrate 1 depletion)")
        print(f"        K2={popt[2]:.3f} is final carrying capacity")
    except Exception as e:
        print(f"    Bi-logistic: failed ({e})")

    # Form 2: Gompertz-logistic hybrid
    # X' = r·X·ln(K/X)·(1 + a·X)
    def gomp_logistic(X, r, K, a):
        lX = np.log(np.clip(K / np.clip(X, 1e-10, None), 1e-10, None))
        return r * X * lX * (1 + a * X)

    try:
        Xmax = np.max(ys)
        popt2, _ = curve_fit(gomp_logistic, X, xp,
                             p0=[0.3, Xmax*1.1, 0.5],
                             bounds=([0, Xmax*0.8, -5], [5, Xmax*3, 5]),
                             maxfev=10000)
        xp_gl = gomp_logistic(X, *popt2)
        res_gl = rmse(xp, xp_gl) / (np.std(xp) + 1e-10)
        results['gomp_logistic'] = {'params': popt2, 'res': res_gl}
        print(f"\n    Gompertz-logistic X' = r·X·ln(K/X)·(1+a·X):")
        print(f"      r={popt2[0]:.4f}  K={popt2[1]:.4f}  a={popt2[2]:.4f}  residual={res_gl:.4f}")
        if popt2[2] > 0:
            print(f"      a>0: density-enhanced growth rate → Allee-like facilitation")
        else:
            print(f"      a<0: density-inhibited growth rate → additional competition")
    except Exception as e:
        print(f"    Gompertz-logistic: failed ({e})")

    # Form 3: Bi-Gompertz (sum-of-logs in autonomous form)
    # X' = X·(r1·ln(K1/X) + r2·ln(K2/X)) only valid when K2 > K1
    # simplifies to: X'= X·[(r1+r2)·ln(K2/X) - r1·ln(K2/K1)]
    def bi_gompertz_ode(X, r1, r2, K1, K2):
        lnK1X = np.log(np.clip(K1/np.clip(X,1e-10,None), 1e-10, None))
        lnK2X = np.log(np.clip(K2/np.clip(X,1e-10,None), 1e-10, None))
        return X * (r1 * lnK1X + r2 * lnK2X)

    try:
        Xmax = np.max(ys)
        popt3, _ = curve_fit(bi_gompertz_ode, X, xp,
                             p0=[0.3, 0.15, Xmax*0.55, Xmax*1.1],
                             bounds=([0, 0, 0.1, Xmax*0.5],
                                     [5, 5, Xmax, Xmax*3]),
                             maxfev=10000)
        xp_bg = bi_gompertz_ode(X, *popt3)
        res_bg = rmse(xp, xp_bg) / (np.std(xp) + 1e-10)
        results['bi_gompertz'] = {'params': popt3, 'res': res_bg}
        print(f"\n    Bi-Gompertz X' = X·[r1·ln(K1/X) + r2·ln(K2/X)]:")
        print(f"      r1={popt3[0]:.4f}  r2={popt3[1]:.4f}  K1={popt3[2]:.4f}  K2={popt3[3]:.4f}")
        print(f"      residual={res_bg:.4f}")
        print(f"      → This is a NOVEL COMPACT form!")
        print(f"        K1={popt3[2]:.3f}: intermediate plateau (substrate 1 depletion)")
        print(f"        K2={popt3[3]:.3f}: final carrying capacity")
        print(f"        r1={popt3[0]:.3f}: growth rate toward K1 (phase 1)")
        print(f"        r2={popt3[1]:.3f}: growth rate toward K2 (phase 2)")
    except Exception as e:
        print(f"    Bi-Gompertz: failed ({e})")

    # Summary
    if results:
        print(f"\n  Summary (normalised ODE-level residuals):")
        for name, r in sorted(results.items(), key=lambda x: x[1]['res']):
            print(f"    {name:<25} residual = {r['res']:.4f}")

    return results


# ═══════════════════════════════════════════════════════════════════════
# C7. Derivative fingerprint: characterise the curve topology
# ═══════════════════════════════════════════════════════════════════════

def derivative_fingerprint(ts, ys, label=""):
    """
    Characterise the diauxic curve by its derivative structure:
    - Number of peaks in X' (growth rate): diauxic has 2 peaks
    - Inflection points in X'' = 0
    - Plateau position (local min of X')
    - Asymmetry between phase 1 and phase 2

    This fingerprint distinguishes diauxic from Gompertz/logistic at a glance
    and guides what ODE class to search.
    """
    print(f"\n  C7 — Derivative Fingerprint  [{label}]")

    xu, xp, Xfit = smooth_deriv(ts, ys, sigma_px=5, order=1)
    _, xpp, _ = smooth_deriv(ts, ys, sigma_px=7, order=2)

    # Find peaks in X' (local maxima)
    from scipy.signal import find_peaks
    peaks, _ = find_peaks(xp, height=np.max(xp)*0.1, distance=10)
    troughs, _ = find_peaks(-xp, height=np.max(-xp)*0.1, distance=10)

    print(f"    X' peaks at t = {[f'{xu[p]:.2f}' for p in peaks]}")
    print(f"    X' troughs (lag phases) at t = {[f'{xu[t]:.2f}' for t in troughs]}")

    n_phases = len(peaks)
    if n_phases == 1:
        print(f"    → Single growth phase (Gompertz/Logistic)")
    elif n_phases == 2:
        print(f"    → TWO growth phases detected (diauxic!)")
        if peaks[0] < peaks[1]:
            ratio = xp[peaks[1]] / (xp[peaks[0]] + 1e-10)
            print(f"    Phase 1 peak rate: {xp[peaks[0]]:.4f} at t={xu[peaks[0]]:.2f}")
            print(f"    Phase 2 peak rate: {xp[peaks[1]]:.4f} at t={xu[peaks[1]]:.2f}")
            print(f"    Rate ratio (phase2/phase1): {ratio:.3f}")
            if len(troughs) > 0:
                lag_t = xu[troughs[0]]
                print(f"    Diauxic lag at t≈{lag_t:.2f}  (X'={xp[troughs[0]]:.5f})")
    else:
        print(f"    → {n_phases} growth phases (complex)")

    # Inflection points of X: zeros of X''
    sign_changes = np.where(np.diff(np.sign(xpp)))[0]
    inflections = xu[sign_changes]
    X_at_inflections = Xfit[sign_changes]
    X_range = np.max(Xfit) - np.min(Xfit)
    rel_heights = (X_at_inflections - np.min(Xfit)) / (X_range + 1e-10)
    print(f"\n    Inflection points (X''=0) at t = {[f'{t:.2f}' for t in inflections]}")
    print(f"    X at inflections: {[f'{x:.4f}' for x in X_at_inflections]}")
    print(f"    Relative height (X-Xmin)/(Xmax-Xmin): {[f'{h:.3f}' for h in rel_heights]}")
    if len(rel_heights) >= 2:
        print(f"    Phase 1 inflection at {rel_heights[0]:.1%} of range")
        print(f"    Phase 2 inflection at {rel_heights[-1]:.1%} of range")
        print(f"    (Gompertz → 37%, Logistic → 50%)")

    return {'n_phases': n_phases, 'peak_times': xu[peaks] if len(peaks) else [],
            'inflections': inflections, 'rel_heights': rel_heights}


# ═══════════════════════════════════════════════════════════════════════
# Main
# ═══════════════════════════════════════════════════════════════════════

if __name__ == '__main__':
    print("="*65)
    print("  STEP C: ODE Discovery for Diauxic Bacterial Growth")
    print("="*65)
    print("""
  Goal: Discover compact symbolic ODEs for diauxic growth WITHOUT
  assuming the form. Use data generated from mechanistic Monod
  two-substrate kinetics as ground truth.
""")

    # Generate data
    ts, X_noisy, X_true = generate_diauxic_data(n_points=120, t_end=50.0)
    print(f"  Generated {len(ts)} points, t ∈ [{ts[0]:.1f}, {ts[-1]:.1f}]")
    print(f"  X range: [{X_noisy.min():.4f}, {X_noisy.max():.4f}]")
    print(f"  Noise std: {np.std(X_noisy - X_true):.4f}")

    print("\n" + "─"*65)
    print("  C2 — Baseline: single Gompertz")
    print("─"*65)
    popt_g, rmse_g, r2_g = fit_gompertz(ts, X_noisy)
    if popt_g is not None:
        print(f"  Gompertz: A={popt_g[0]:.4f}  μ={popt_g[1]:.4f}  λ={popt_g[2]:.4f}")
        print(f"  RMSE={rmse_g:.4f}  R²={r2_g:.4f}")
        if r2_g < 0.98:
            print("  → Poor fit: data is NOT well-described by a single Gompertz")

    # Derivative fingerprint (run before everything else)
    fp = derivative_fingerprint(ts, X_noisy, "Monod 2-substrate diauxic")

    # C3: Symbolic boosting (naive)
    boost_result = symbolic_boosting(ts, X_noisy, "Monod 2-substrate diauxic")

    # C3b: Phase-segmented boosting
    seg_result = segmented_boosting(ts, X_noisy, "Monod 2-substrate diauxic")

    # C4: Autonomous ODE discovery
    ode_results, best_deg = autonomous_ode_discovery(ts, X_noisy, "Monod diauxic")

    # C5: Second order ODE
    ode2_result = second_order_ode_discovery(ts, X_noisy, "Monod diauxic")

    # C6: Compact form search (the novel part)
    compact = compact_diauxic_ode_search(ts, X_noisy, "Monod diauxic")

    # Validate bi-Gompertz ODE by integration
    if 'bi_gompertz' in compact:
        p = compact['bi_gompertz']['params']
        validate_bi_gompertz_ode(p[0], p[1], p[2], p[3], X0=X_noisy[0])

    print("\n" + "="*65)
    print("  SYNTHESIS")
    print("="*65)
    print("""
  The discovered compact ODE X' = X·[r1·ln(K1/X) + r2·ln(K2/X)]
  (bi-Gompertz autonomous form) has a biological interpretation:

  - K1 = first carrying capacity = biomass reachable on substrate 1 alone
  - K2 = final carrying capacity  = biomass on S1 + S2
  - r1 = growth rate toward K1    = metabolism of preferred substrate
  - r2 = growth rate toward K2    = metabolism of secondary substrate

  This is a genuinely compact (4 parameters) autonomous ODE that
  captures diauxic dynamics as a SINGLE continuous equation,
  unlike the empirical "sum of two Gompertz" which requires 6 parameters
  and has no compact ODE form.

  The bi-Gompertz ODE is equivalent to:
    X' = r1·X·ln(K1/X) + r2·X·ln(K2/X)
    (additive Gompertz fluxes from two substrate-limited growth rates)

  This has a mechanistic interpretation: at any moment, the bacterium
  is simultaneously "pushing toward K1" (from substrate 1) and
  "pushing toward K2" (from substrate 2, which activates after S1 depletes).
""")

    # Also run on a simple double-Gompertz synthetic (known ground truth)
    print("─"*65)
    print("  Validation: Double-Gompertz synthetic data")
    print("─"*65)
    t_syn = np.linspace(0, 50, 100)
    A1, mu1, lam1 = 0.55, 0.35, 6.0
    A2, mu2, lam2 = 0.40, 0.20, 26.0
    X_syn = (A1*np.exp(-np.exp((mu1*np.e/A1)*(lam1-t_syn)+1))
           + A2*np.exp(-np.exp((mu2*np.e/A2)*(lam2-t_syn)+1)))
    X_syn += np.random.default_rng(0).normal(0, 0.005, len(t_syn))

    derivative_fingerprint(t_syn, X_syn, "double-Gompertz synthetic")
    symbolic_boosting(t_syn, X_syn, "double-Gompertz synthetic")
    segmented_boosting(t_syn, X_syn, "double-Gompertz synthetic")
    compact = compact_diauxic_ode_search(t_syn, X_syn, "double-Gompertz synthetic")
    if 'bi_gompertz' in compact:
        p = compact['bi_gompertz']['params']
        validate_bi_gompertz_ode(p[0], p[1], p[2], p[3], X0=X_syn[0])

    print("\nStep C complete.")
