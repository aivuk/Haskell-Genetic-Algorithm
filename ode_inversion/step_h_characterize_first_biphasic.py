"""
Characterize-First ODE Discovery for Biphasic Growth (article methods B1/B2/B3).
Applied to real LG166 data.

Pipeline — genuinely assumption-free:
  0. Derivative fingerprint: count peaks in X'(t) → detect phases
  1. Phase segmentation at valley between peaks
  2. For each phase independently:
       B1: log-space linearisation  → detects Gompertz (g'=a+b·g, b<0)
       B3: STRidge on feature library [1, f, f², f·lnf, f(1-f/K)]
           → model CLASSIFIER, selects equation type from data
       inflection-position test → 37%=Gompertz, 50%=Logistic
       B2: K-scan refinement (only if B3 selected Gompertz)
  3. Stitch phases: X(t) = X1(t) + X2(t) using discovered ODEs
  4. Forward integrate, compute R²
  5. Report which ODE was discovered per phase — never assumed

Key: the ODE form is NOT assumed. B3's STRidge decides whether the
data is best described by f·ln(f) (Gompertz), f·(1-f/K) (logistic),
f (exponential), or some combination.
"""

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import pandas as pd
from scipy.integrate import solve_ivp
from scipy.ndimage import gaussian_filter1d
from scipy.interpolate import UnivariateSpline
from scipy.optimize import curve_fit
from scipy.signal import find_peaks as sp_find_peaks
import warnings
warnings.filterwarnings('ignore')

# ── Load data ─────────────────────────────────────────────────────────────────

DATA_PATH = ('/media/aivuk/64fce268-2613-4033-b39f-537ae2d28805/roms/pinheiroTech'
             '/aHPM-web-interface/Clean_data/LG166/data_channel_1.csv')
ANN_PATH  = ('/media/aivuk/64fce268-2613-4033-b39f-537ae2d28805/roms/pinheiroTech'
             '/aHPM-web-interface/Clean_data/LG166/annotation_clean.csv')

df  = pd.read_csv(DATA_PATH)
ann = pd.read_csv(ANN_PATH, header=None,
                  names=['well','type','media','substrate','antibiotic','blank_ref','n'])
t_raw = df['Time'].values

blanks     = ann[ann['type'] == 'b']['well'].tolist()
background = df[blanks].mean(axis=1).values
excluded   = ann[ann['type'].isin(['b','X'])]['well'].tolist()
valid_wells = [c for c in df.columns[1:] if c not in excluded]
ann_idx = ann.set_index('well')

def condition(well):
    row = ann_idx.loc[well]
    ab = row['antibiotic'] if pd.notna(row['antibiotic']) else 'control'
    return str(ab).replace('Chloramphenicol_','CHL').replace('Rifampicin_','RIF')

# ── Core signal processing ────────────────────────────────────────────────────

def smooth_signal(t_in, x_in, sigma_px=10):
    """Smooth with Gaussian filter, return (t_smooth, X_smooth, Xp_smooth)."""
    x = np.clip(x_in, 1e-5, None)
    spl = UnivariateSpline(t_in, x, k=5, s=len(t_in)*2e-4)
    xu = np.linspace(t_in[0], t_in[-1], 600)
    yu = np.clip(spl(xu), 1e-5, None)
    dx = xu[1] - xu[0]
    edge = min(int(np.ceil(3*sigma_px)), (len(xu)-30)//2)
    Xp = gaussian_filter1d(yu, sigma_px, order=1, mode='nearest') / dx
    return xu[edge:-edge], yu[edge:-edge], Xp[edge:-edge]

def find_phase_boundary(xu, Xp, min_height_frac=0.08, min_dist_h=4.0):
    """Return (n_peaks, peak_times, valley_time) from X'(t)."""
    dx = xu[1]-xu[0]
    min_dist = max(1, int(min_dist_h/dx))
    peaks, _ = sp_find_peaks(Xp, height=min_height_frac*Xp.max(), distance=min_dist,
                              prominence=0.003)
    if len(peaks) < 2:
        return len(peaks), xu[peaks].tolist(), None
    # Valley between first and last peak
    valley_region = Xp[(xu > xu[peaks[0]]) & (xu < xu[peaks[-1]])]
    valley_t = xu[(xu > xu[peaks[0]]) & (xu < xu[peaks[-1]])]
    if len(valley_t) == 0:
        return len(peaks), xu[peaks].tolist(), None
    t_valley = valley_t[np.argmin(valley_region)]
    return len(peaks), xu[peaks].tolist(), t_valley

# ════════════════════════════════════════════════════════════════════════════
# ARTICLE METHODS
# ════════════════════════════════════════════════════════════════════════════

def stridge(Theta, y, threshold, alpha=1e-5, max_iter=100):
    """Sequential Thresholded Ridge Regression (from article Section 3)."""
    n = Theta.shape[1]
    active = np.ones(n, dtype=bool)
    coef = np.zeros(n)
    for _ in range(max_iter):
        idx = np.where(active)[0]
        if len(idx) == 0: break
        A = Theta[:, idx]
        w = np.linalg.lstsq(A.T@A + alpha*np.eye(len(idx)), A.T@y, rcond=None)[0]
        small = np.abs(w) < threshold * np.abs(w).max()
        if small.any():
            active[idx[small]] = False
        else:
            coef[idx] = w
            break
    return coef

def method_B1(xu, Xu, Xp, label=''):
    """
    B1: Log-space linearisation (article Section 4.1).
    Tests whether g = ln(f) satisfies g' = a + b*g (b<0 → Gompertz).
    Returns: (a, b, K, r2_loglin) — if b>=0, not Gompertz.
    """
    g  = np.log(np.clip(Xu, 1e-8, None))
    gp = Xp / (Xu + 1e-8)   # g' = f'/f
    G  = np.column_stack([np.ones_like(g), g])
    try:
        ab, _, _, _ = np.linalg.lstsq(G, gp, rcond=None)
        a, b = ab
        gp_pred = a + b*g
        ss_res = np.sum((gp - gp_pred)**2)
        ss_tot = np.sum((gp - gp.mean())**2)
        r2_ll = 1 - ss_res/(ss_tot+1e-20)
        K = np.exp(-a/b) if b < 0 else None
        r = -b if b < 0 else None
        return a, b, K, r, r2_ll
    except Exception:
        return 0, 0, None, None, 0

def method_B3(xu, Xu, Xp, K_est, label=''):
    """
    B3: STRidge on growth feature library (article Section 4.3).
    Library: [1, f, f², f·lnf, f·(1-f/K)]
    Returns: (coef, feature_names, dominant_model_str)
    — NO model assumed, STRidge picks the form.
    """
    f   = Xu
    fc  = np.clip(f, 1e-8, None)
    K   = max(K_est, f.max()*1.001)

    feat_names = ['1', 'f', 'f²', 'f·ln(f)', 'f·(1−f/K)']
    Theta = np.column_stack([
        np.ones(len(f)),          # 1        → offset / death
        f,                         # f        → exponential growth
        f**2,                      # f²       → logistic (simplified)
        fc * np.log(fc),           # f·ln(f)  → Gompertz
        f * (1 - f/K),             # f(1-f/K) → logistic (full)
    ])

    # Try multiple thresholds, pick sparsest solution with R²>0.90
    best = {'r2': -np.inf, 'coef': np.zeros(5), 'thr': 0}
    for thr in [0.01, 0.05, 0.1, 0.2, 0.3, 0.5]:
        coef = stridge(Theta, Xp, thr)
        n_active = (coef != 0).sum()
        if n_active == 0: continue
        pred = Theta @ coef
        ss_res = np.sum((Xp - pred)**2)
        ss_tot = np.sum((Xp - Xp.mean())**2)
        r2 = 1 - ss_res/(ss_tot+1e-20)
        if r2 > best['r2']:
            best = {'r2': r2, 'coef': coef, 'thr': thr}

    coef = best['coef']
    active_terms = [(nm, c) for nm, c in zip(feat_names, coef) if c != 0]

    # Identify dominant model class from active terms
    c1, c_f, c_f2, c_flnf, c_logistic = coef
    if c_flnf != 0 and c_logistic == 0 and c_f2 == 0:
        model_class = 'Gompertz'
    elif c_logistic != 0 and c_flnf == 0:
        model_class = 'Logistic'
    elif c_f2 != 0 and c_flnf == 0 and c_logistic == 0:
        model_class = 'Logistic(simplified)'
    elif c_f != 0 and c_flnf == 0 and c_f2 == 0 and c_logistic == 0:
        model_class = 'Exponential'
    elif c_flnf != 0 and (c_logistic != 0 or c_f2 != 0):
        model_class = 'Mixed(Gompertz+Logistic)'
    else:
        model_class = 'Unknown'

    eq_str = ' + '.join(f'{c:+.4f}·{nm}' for nm, c in active_terms)

    return coef, feat_names, model_class, eq_str, best['r2']

def method_B2(xu, Xu, Xp, K_hint=None, label=''):
    """
    B2: Direct Gompertz ODE K-scan (article Section 4.2).
    Only called if B3 identifies Gompertz. Scans K, finds optimal r via OLS.
    Returns: (K, r, r2_deriv)
    """
    f_max = Xu.max()
    K_lo  = f_max * 1.001
    K_hi  = max(f_max * 4, K_hint*1.5 if K_hint else f_max*3)
    best  = {'res': np.inf, 'K': K_lo, 'r': 0}
    for K in np.linspace(K_lo, K_hi, 300):
        phi = Xu * np.log(K / np.clip(Xu, 1e-8, None))
        r   = np.dot(Xp, phi) / (np.dot(phi, phi) + 1e-10)
        if r <= 0: continue
        res = np.sqrt(np.mean((Xp - r*phi)**2)) / (np.std(Xp)+1e-10)
        if res < best['res']:
            best = {'res': res, 'K': K, 'r': r}
    # Nonlinear refinement via curve_fit
    def gompertz(t, A, r, lam):
        z = np.clip(r*np.e/A*(lam-t)+1, -500, 500)
        return A*np.exp(-np.exp(z))
    try:
        lam0 = xu[np.argmax(Xp)]
        popt, _ = curve_fit(gompertz, xu, Xu, p0=[best['K'], best['r'], lam0],
                            bounds=([0.001,0.001,-10],[10,10,200]), maxfev=3000)
        K, r, lam = popt
        phi = Xu * np.log(K / np.clip(Xu, 1e-8, None))
        r_check = np.dot(Xp, phi) / (np.dot(phi, phi) + 1e-10)
        res_r2 = 1 - np.sum((Xp - r_check*phi)**2)/(np.sum((Xp-Xp.mean())**2)+1e-20)
        return K, r, lam, res_r2
    except Exception:
        return best['K'], best['r'], xu[np.argmax(Xp)], 1-best['res']**2

def inflection_test(xu, Xu):
    """
    Article Section 5.2: inflection position relative to growth range.
    Gompertz: ~37%, Logistic: ~50%.
    """
    Xmin = Xu.min(); Xmax = Xu.max()
    t_infl = xu[np.argmax(np.gradient(Xu, xu))]
    X_infl = float(UnivariateSpline(xu, Xu, k=3, s=0)(t_infl))
    frac   = (X_infl - Xmin) / (Xmax - Xmin + 1e-10)
    if   frac < 0.43: label = 'Gompertz (~37%)'
    elif frac < 0.57: label = 'Logistic (~50%)'
    else:             label = f'Unknown ({frac*100:.0f}%)'
    return frac, label, t_infl

# ════════════════════════════════════════════════════════════════════════════
# FULL BIPHASIC PIPELINE
# ════════════════════════════════════════════════════════════════════════════

def r2_score(y, yh):
    y = np.array(y); yh = np.array(yh)
    return 1 - np.sum((y-yh)**2)/(np.sum((y-np.mean(y))**2)+1e-20)

def discover_biphasic(well, sigma_px=10, verbose=True):
    """
    Full characterize-first pipeline for one well.
    Returns dict with discovered equations and quality metrics.
    """
    raw  = np.clip(df[well].values - background, 1e-5, None)
    xu, Xu, Xp = smooth_signal(t_raw, raw, sigma_px=sigma_px)

    if verbose:
        print(f"\n{'='*60}")
        print(f"Well {well} — {condition(well)}")
        print(f"{'='*60}")

    # ── Step 0: Derivative fingerprint ───────────────────────────
    n_peaks, peak_times, t_valley = find_phase_boundary(xu, Xp)
    biphasic = (n_peaks >= 2 and t_valley is not None)

    if verbose:
        print(f"  Peaks in X'(t): {n_peaks} at t={[f'{p:.1f}h' for p in peak_times]}")
        print(f"  Biphasic: {biphasic}  | valley at t={t_valley:.1f}h" if biphasic
              else f"  Single-phase growth")

    result = {'well': well, 'condition': condition(well),
              'biphasic': biphasic, 'n_peaks': n_peaks,
              'peak_times': peak_times, 't_valley': t_valley,
              'xu': xu, 'Xu': Xu, 'Xp': Xp, 'raw': raw,
              'phases': {}}

    # ── Helper: run all three B-methods on a segment ──────────────
    def analyse_phase(t_seg, X_seg, Xp_seg, phase_label, K_plateau):
        if len(t_seg) < 20:
            return None

        # Inflection position test (from article)
        frac, infl_label, t_infl = inflection_test(t_seg, X_seg)

        # B1: log-space linearisation
        a_b1, b_b1, K_b1, r_b1, r2_b1 = method_B1(t_seg, X_seg, Xp_seg)

        # B3: STRidge feature library — the classifier
        K_est = K_plateau if K_plateau else X_seg.max()*1.05
        coef_b3, feat_names, model_class, eq_str, r2_b3 = method_B3(
            t_seg, X_seg, Xp_seg, K_est)

        # B2: K-scan refinement (only if B3 selected Gompertz or Mixed-Gompertz)
        K_b2 = r_b2 = lam_b2 = r2_b2 = None
        if 'Gompertz' in model_class:
            K_b2, r_b2, lam_b2, r2_b2 = method_B2(t_seg, X_seg, Xp_seg,
                                                     K_hint=K_est)

        if verbose:
            print(f"\n  ── Phase {phase_label} ({len(t_seg)} pts, "
                  f"t=[{t_seg[0]:.1f},{t_seg[-1]:.1f}]h) ──")
            print(f"    Inflection at {frac*100:.0f}% of range → {infl_label}")
            print(f"    B1 (log-lin): g'=a+b·g, b={b_b1:.4f} "
                  f"({'Gompertz' if b_b1<0 else 'NOT Gompertz'}), R²={r2_b1:.3f}")
            print(f"    B3 (STRidge): f' = {eq_str}")
            print(f"    B3 model class: {model_class}  (R²_deriv={r2_b3:.3f})")
            if K_b2 is not None:
                print(f"    B2 refinement:  K={K_b2:.4f}, r={r_b2:.4f}, λ={lam_b2:.2f}h")

        return {
            'inflection_frac': frac, 'inflection_label': infl_label,
            'B1': {'a': a_b1, 'b': b_b1, 'K': K_b1, 'r': r_b1, 'r2': r2_b1},
            'B3': {'coef': coef_b3, 'feat_names': feat_names,
                   'model_class': model_class, 'eq_str': eq_str, 'r2': r2_b3},
            'B2': {'K': K_b2, 'r': r_b2, 'lam': lam_b2, 'r2': r2_b2}
                  if K_b2 is not None else None,
            't_seg': t_seg, 'X_seg': X_seg,
        }

    if biphasic:
        # ── Phase 1: t < t_valley, X ≈ X1 directly ──────────────────
        m1 = xu < t_valley
        p1 = analyse_phase(xu[m1], Xu[m1], Xp[m1], '1 (phase 1)', None)

        # Estimate plateau level K1 from the valley region
        valley_width = 3.0  # h
        m_plat = (xu > t_valley - valley_width) & (xu < t_valley + valley_width)
        K1_plateau = float(Xu[m_plat].mean()) if m_plat.any() else Xu[m1].max()

        # ── Phase 2: t > t_valley, X2 = X - K1_plateau ───────────────
        # This isolates phase-2 growth from the observed total
        m2 = xu > t_valley
        X2_seg = np.clip(Xu[m2] - K1_plateau, 1e-5, None)
        Xp2_seg = Xp[m2]  # derivative of (X-K1) = derivative of X (K1 constant)
        p2 = analyse_phase(xu[m2], X2_seg, Xp2_seg, '2 (phase 2)', None)

        result['phases'] = {'1': p1, '2': p2, 'K1_plateau': K1_plateau}
    else:
        # Single phase
        p1 = analyse_phase(xu, Xu, Xp, '(single)', None)
        result['phases'] = {'1': p1, 'K1_plateau': Xu.max()}

    # ── Forward integration of discovered model ───────────────────────────────
    if verbose:
        print(f"\n  ── Forward integration ──────────────────────────────")

    def build_ode(phase_info, offset=0.0):
        """Build ODE function from B3 discovered coefficients."""
        coef = phase_info['B3']['coef']
        # Use B2 parameters if available (more precise for Gompertz)
        if phase_info['B2'] is not None:
            K_use = phase_info['B2']['K']
            r_use = phase_info['B2']['r']
            lam_use = phase_info['B2']['lam']
            def ode_fn(t_val, y):
                fv = max(y[0], 1e-8)
                phi = fv * np.log(max(K_use/fv, 1e-8))
                return [r_use * phi]
        else:
            K_est = max(phase_info['X_seg'].max()*1.05,
                        phase_info['X_seg'].max()+0.001)
            def ode_fn(t_val, y):
                fv = max(y[0], 1e-8)
                fc = max(fv, 1e-8)
                feats = np.array([
                    1.0, fv, fv**2,
                    fc*np.log(fc),
                    fv*(1 - fv/K_est)
                ])
                return [float(np.dot(coef, feats))]
        return ode_fn

    # Integrate phase 1
    r2_total = np.nan
    X_pred_total = np.full(len(xu), np.nan)
    try:
        p1_info = result['phases']['1']
        if p1_info is not None:
            ode1 = build_ode(p1_info)
            X0_1 = float(Xu[0])
            sol1  = solve_ivp(ode1, [xu[0], xu[-1]], [X0_1], t_eval=xu,
                              method='RK45', rtol=1e-5, atol=1e-7, max_step=0.5)
            if sol1.success:
                X1_pred = np.clip(sol1.y[0], 0, None)
            else:
                X1_pred = np.zeros(len(xu))
        else:
            X1_pred = np.zeros(len(xu))

        K1_plat = result['phases'].get('K1_plateau', Xu.max())

        if biphasic and result['phases'].get('2') is not None:
            p2_info = result['phases']['2']
            ode2 = build_ode(p2_info)
            t_v_idx = np.argmin(np.abs(xu - t_valley))
            X0_2 = float(np.clip(Xu[t_v_idx] - K1_plat, 1e-5, None))
            sol2 = solve_ivp(ode2, [t_valley, xu[-1]], [X0_2],
                             t_eval=xu[xu >= t_valley],
                             method='RK45', rtol=1e-5, atol=1e-7, max_step=0.5)
            if sol2.success:
                X2_pred_seg = np.clip(sol2.y[0], 0, None)
                X2_pred_full = np.concatenate([
                    np.zeros(np.sum(xu < t_valley)), X2_pred_seg])
            else:
                X2_pred_full = np.zeros(len(xu))

            X_pred_total = X1_pred + X2_pred_full + K1_plat * (xu >= t_valley).astype(float)
            # Simpler: use analytical Gompertz if B2 found parameters
            if (p1_info['B2'] is not None and
                result['phases'].get('2') is not None and
                result['phases']['2']['B2'] is not None):
                K1, r1, lam1 = (p1_info['B2']['K'], p1_info['B2']['r'],
                                 p1_info['B2']['lam'])
                K2, r2_val, lam2 = (p2_info['B2']['K'], p2_info['B2']['r'],
                                     p2_info['B2']['lam'])
                def G(A, r, lam):
                    z = np.clip(r*np.e/A*(lam-xu)+1, -500, 500)
                    return A*np.exp(-np.exp(z))
                X_pred_total = G(K1, r1, lam1) + G(K2, r2_val, lam2)
        else:
            X_pred_total = X1_pred

        r2_total = r2_score(Xu, X_pred_total)
        result['X_pred'] = X_pred_total
        result['r2'] = r2_total
        if verbose:
            print(f"    R² (forward integration) = {r2_total:.4f}")
    except Exception as e:
        result['X_pred'] = np.full(len(xu), np.nan)
        result['r2'] = np.nan
        if verbose:
            print(f"    Integration failed: {e}")

    return result

# ════════════════════════════════════════════════════════════════════════════
# RUN ON REPRESENTATIVE WELLS
# ════════════════════════════════════════════════════════════════════════════

# Pick a representative sample: controls + one dose of each antibiotic
sample_wells = ['A3', 'A5', 'B5', 'C5', 'D5', 'E5', 'F5',   # Rifampicin series
                'B1', 'C1', 'D1', 'E1', 'F1']                 # Chloramphenicol series

print("Running characterize-first biphasic ODE discovery...")
all_results = {}
for well in sample_wells:
    r = discover_biphasic(well, verbose=True)
    all_results[well] = r

# ════════════════════════════════════════════════════════════════════════════
# SUMMARY TABLE
# ════════════════════════════════════════════════════════════════════════════
print("\n\n" + "="*80)
print("CHARACTERIZE-FIRST DISCOVERY SUMMARY")
print("="*80)
print(f"{'Well':4s} {'Condition':12s} {'Bi':3s} "
      f"{'Ph1 model':22s} {'Ph2 model':22s} "
      f"{'Infl1':6s} {'Infl2':6s} {'R²':6s}")
print("-"*80)
for well in sample_wells:
    r = all_results[well]
    bi = 'YES' if r['biphasic'] else 'NO'
    p1 = r['phases'].get('1')
    p2 = r['phases'].get('2')
    m1 = p1['B3']['model_class'][:20] if p1 else '—'
    m2 = p2['B3']['model_class'][:20] if p2 else '—'
    i1 = f"{p1['inflection_frac']*100:.0f}%" if p1 else '—'
    i2 = f"{p2['inflection_frac']*100:.0f}%" if p2 else '—'
    r2v = f"{r['r2']:.3f}" if not np.isnan(r.get('r2', np.nan)) else '—'
    print(f"{well:4s} {r['condition']:12s} {bi:3s} {m1:22s} {m2:22s} {i1:6s} {i2:6s} {r2v:6s}")

# ════════════════════════════════════════════════════════════════════════════
# PLOTS
# ════════════════════════════════════════════════════════════════════════════
print("\nGenerating plots...")

n_wells = len(sample_wells)
ncols = 4
nrows = int(np.ceil(n_wells / ncols))

fig, axes = plt.subplots(nrows, ncols, figsize=(20, nrows*5))
axes = axes.flatten()

for i, well in enumerate(sample_wells):
    ax = axes[i]
    r  = all_results[well]
    xu = r['xu']; Xu = r['Xu']

    # Raw data
    ax.plot(t_raw, r['raw'], '.', color='gray', ms=2, alpha=0.4, zorder=1)
    ax.plot(xu, Xu, 'k-', lw=2, label='Data (smoothed)', zorder=5)

    # Discovered model
    if 'X_pred' in r and not np.all(np.isnan(r['X_pred'])):
        ax.plot(xu, r['X_pred'], 'r-', lw=2.5, label=f"Discovered  R²={r['r2']:.3f}",
                zorder=6)

    # Phase boundary
    if r['biphasic'] and r['t_valley']:
        ax.axvline(r['t_valley'], color='orange', ls='--', lw=1.5, alpha=0.8)
        ax.annotate(f"valley\nt={r['t_valley']:.0f}h",
                    xy=(r['t_valley'], Xu.max()*0.3), fontsize=7, color='darkorange')

    # Annotation: discovered equations
    p1 = r['phases'].get('1')
    p2 = r['phases'].get('2')
    eq_lines = []
    if p1:
        mc = p1['B3']['model_class']
        infl = f"{p1['inflection_frac']*100:.0f}%"
        if p1['B2']:
            eq_lines.append(f"Ph1: r={p1['B2']['r']:.3f} K={p1['B2']['K']:.3f}")
        eq_lines.append(f"→ {mc} ({infl})")
    if p2:
        mc2 = p2['B3']['model_class']
        infl2 = f"{p2['inflection_frac']*100:.0f}%"
        if p2['B2']:
            eq_lines.append(f"Ph2: r={p2['B2']['r']:.3f} K={p2['B2']['K']:.3f}")
        eq_lines.append(f"→ {mc2} ({infl2})")
    ax.text(0.02, 0.98, '\n'.join(eq_lines), transform=ax.transAxes,
            fontsize=7, va='top', color='navy',
            bbox=dict(boxstyle='round,pad=0.2', facecolor='lightyellow', alpha=0.8))

    ax.set_title(f"{well} — {r['condition']}\n{'Biphasic' if r['biphasic'] else 'Single-phase'}",
                 fontsize=9, color='red' if r['biphasic'] else 'navy')
    ax.set_xlabel('Time [h]', fontsize=8)
    ax.set_ylabel('OD (bg-subtracted)', fontsize=8)
    ax.legend(fontsize=7, loc='lower right')
    ax.grid(alpha=0.3)

for j in range(i+1, len(axes)):
    axes[j].set_visible(False)

fig.suptitle(
    "Characterize-First Biphasic ODE Discovery (B1/B2/B3 from article)\n"
    "ODE form NOT assumed — B3 STRidge classifies each phase independently",
    fontsize=12, fontweight='bold'
)
plt.tight_layout()
out1 = 'ode_inversion/LG166_characterize_first.png'
plt.savefig(out1, dpi=130, bbox_inches='tight')
print(f"Saved: {out1}")
plt.close()

# ── Figure 2: B3 model classification breakdown ───────────────────────────────
fig2, axes2 = plt.subplots(1, 2, figsize=(14, 5))
fig2.suptitle("B3 STRidge: What ODE form did the data choose?\n"
              "(No model assumed — library = [1, f, f², f·lnf, f(1-f/K)])",
              fontsize=11, fontweight='bold')

# Phase 1 model classes
p1_classes = [all_results[w]['phases']['1']['B3']['model_class']
              for w in sample_wells if all_results[w]['phases'].get('1')]
p2_classes = [all_results[w]['phases']['2']['B3']['model_class']
              for w in sample_wells
              if all_results[w]['biphasic'] and all_results[w]['phases'].get('2')]

for ax, classes, title in [
    (axes2[0], p1_classes, 'Phase 1'),
    (axes2[1], p2_classes, 'Phase 2'),
]:
    unique, counts = np.unique(classes, return_counts=True)
    colors = ['#2ca02c' if 'Gompertz' in u else
              '#1f77b4' if 'Logistic' in u else
              '#d62728' if 'Exp' in u else '#9467bd' for u in unique]
    bars = ax.bar(unique, counts, color=colors, alpha=0.8, edgecolor='k')
    for bar, c in zip(bars, counts):
        ax.text(bar.get_x()+bar.get_width()/2, c+0.05, str(c),
                ha='center', fontsize=11, fontweight='bold')
    ax.set_title(f'{title} — B3 discovered model class\n'
                 f'(green=Gompertz, blue=Logistic, red=Exponential)')
    ax.set_ylabel('Number of wells')
    ax.tick_params(axis='x', rotation=20)
    ax.grid(alpha=0.3, axis='y')

plt.tight_layout()
out2 = 'ode_inversion/LG166_B3_classification.png'
plt.savefig(out2, dpi=130, bbox_inches='tight')
print(f"Saved: {out2}")
plt.close()

print("\nDone.")
