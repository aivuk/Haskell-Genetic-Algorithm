"""
Apply 2-component Gompertz discovery to real growth curve data (LG166).

Dataset: E. coli BW25113 in M9+glucose, with Chloramphenicol / Rifampicin dose-response.
Each well: OD600 time series, t ∈ [0, 49 h], 493 points.

Pipeline for each well:
  1. Background-subtract using blank wells (A1, A7)
  2. Smooth and compute derivative X'(t)
  3. Detect number of peaks → biphasic signature check
  4. Fit 2-component Gompertz: X = X1 + X2 via curve_fit
  5. Report R², discovered parameters, compare to single-Gompertz
  6. Phase portrait proof: check if 1D ODE is insufficient
"""

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import pandas as pd
from scipy.integrate import solve_ivp
from scipy.ndimage import gaussian_filter1d
from scipy.interpolate import UnivariateSpline
from scipy.optimize import curve_fit, minimize
from scipy.signal import find_peaks as sp_find_peaks
import warnings
warnings.filterwarnings('ignore')

# ── Load data ─────────────────────────────────────────────────────────────────

DATA_PATH = ('/media/aivuk/64fce268-2613-4033-b39f-537ae2d28805/roms/pinheiroTech'
             '/aHPM-web-interface/Clean_data/LG166/data_channel_1.csv')
ANN_PATH  = ('/media/aivuk/64fce268-2613-4033-b39f-537ae2d28805/roms/pinheiroTech'
             '/aHPM-web-interface/Clean_data/LG166/annotation_clean.csv')

df   = pd.read_csv(DATA_PATH)
ann  = pd.read_csv(ANN_PATH, header=None,
                   names=['well','type','media','substrate','antibiotic','blank_ref','n'])

t_raw = df['Time'].values

# Background subtraction: mean of blank wells A1, A7 over time
blank_wells = ann[ann['type'] == 'b']['well'].tolist()
background  = df[blank_wells].mean(axis=1).values
print(f"Blank wells: {blank_wells},  mean background OD = {background.mean():.4f}")

# Excluded wells
excluded = ann[ann['type'].isin(['b','X'])]['well'].tolist()
valid_wells = [c for c in df.columns[1:] if c not in excluded]
print(f"Valid wells: {len(valid_wells)} of {len(df.columns)-1}")

# Build condition label for each well
ann_idx = ann.set_index('well')
def condition(well):
    row = ann_idx.loc[well]
    ab = row['antibiotic'] if pd.notna(row['antibiotic']) else 'control'
    return ab

# ── Helper functions ──────────────────────────────────────────────────────────

def preprocess(well, sigma_px=10):
    """Background-subtract, clip, smooth, return (t, X, Xp, Xu)."""
    raw = df[well].values - background
    raw = np.clip(raw, 1e-5, None)
    spl = UnivariateSpline(t_raw, raw, k=5, s=len(t_raw)*2e-4)
    xu  = np.linspace(t_raw[0], t_raw[-1], 600)
    yu  = np.clip(spl(xu), 1e-5, None)
    dx  = xu[1] - xu[0]
    edge = min(int(np.ceil(3*sigma_px)), (len(xu)-30)//2)
    Xp  = gaussian_filter1d(yu, sigma_px, order=1, mode='nearest') / dx
    return xu[edge:-edge], Xp[edge:-edge], yu[edge:-edge], raw

def count_peaks(Xp, xu, min_height_frac=0.08, min_distance_h=3.0):
    """Count meaningful peaks in X'(t)."""
    if Xp.max() <= 0:
        return 0, []
    dx = xu[1] - xu[0]
    min_dist_px = max(1, int(min_distance_h / dx))
    peaks, _ = sp_find_peaks(Xp, height=min_height_frac*Xp.max(),
                              distance=min_dist_px, prominence=0.005)
    return len(peaks), list(xu[peaks])

def G_analytic(t_val, A, r, lam):
    z = np.clip(r * np.e / A * (lam - t_val) + 1, -500, 500)
    return A * np.exp(-np.exp(z))

def r2_score(y, yh):
    y = np.array(y); yh = np.array(yh)
    ss_res = np.sum((y-yh)**2)
    ss_tot = np.sum((y-np.mean(y))**2)
    return 1 - ss_res/(ss_tot + 1e-20)

def fit_single_gompertz(t_fit, X_fit):
    """Fit single Gompertz: X = A*exp(-exp(r*e/A*(lam-t)+1))"""
    def sg(t, A, r, lam):
        return G_analytic(t, A, r, lam)
    try:
        A0 = X_fit.max()
        lam0 = t_fit[np.argmax(np.gradient(X_fit, t_fit))]
        p0 = [A0, 0.3, lam0]
        popt, _ = curve_fit(sg, t_fit, X_fit, p0=p0,
                            bounds=([0.01, 0.01, -5], [5, 10, 200]),
                            maxfev=3000, method='trf')
        Xpred = sg(t_fit, *popt)
        return popt, r2_score(X_fit, Xpred)
    except Exception:
        return None, -np.inf

def fit_double_gompertz(t_fit, X_fit, lam1_hint, lam2_hint):
    """Fit double Gompertz: X = G1 + G2"""
    def dg(t, r1, K1, lam1, r2, K2, lam2):
        return G_analytic(t, K1, r1, lam1) + G_analytic(t, K2, r2, lam2)
    Xmax = X_fit.max()
    try:
        p0 = [0.4, Xmax*0.5, lam1_hint, 0.2, Xmax*0.5, lam2_hint]
        lo = [0.01, 0.01, -5,  0.01, 0.01, 0]
        hi = [10,   Xmax*2, 60, 10,   Xmax*2, 200]
        popt, _ = curve_fit(dg, t_fit, X_fit, p0=p0,
                            bounds=(lo, hi), maxfev=5000, method='trf')
        Xpred = dg(t_fit, *popt)
        return popt, r2_score(X_fit, Xpred)
    except Exception:
        return None, -np.inf

# ── Process all wells ─────────────────────────────────────────────────────────

print("\nProcessing wells...")
results = {}

for well in valid_wells:
    xu, Xp, Xu, raw = preprocess(well)
    n_peaks, peak_times = count_peaks(Xp, xu)
    Xmax = Xu.max()

    # Single Gompertz fit (baseline)
    sg_params, r2_sg = fit_single_gompertz(xu, Xu)

    # Double Gompertz fit
    if n_peaks >= 2:
        lam1_h = peak_times[0]
        lam2_h = peak_times[-1]
    else:
        # For single-peak: try split at 60% of max growth time
        lam1_h = peak_times[0] if peak_times else xu[len(xu)//3]
        lam2_h = xu[2*len(xu)//3]

    dg_params, r2_dg = fit_double_gompertz(xu, Xu, lam1_h, lam2_h)

    # Phase portrait: is μ(X) multi-valued?
    Mu = Xp / (Xu + 1e-6)
    t_valley_idx = np.argmin(Xp[len(Xp)//4:3*len(Xp)//4]) + len(Xp)//4
    t_valley = xu[t_valley_idx]
    mask1 = xu < t_valley; mask2 = xu > t_valley
    X_ov_lo = max(Xu[mask1].min(), Xu[mask2].min()) if mask1.any() and mask2.any() else 0
    X_ov_hi = min(Xu[mask1].max(), Xu[mask2].max()) if mask1.any() and mask2.any() else 0
    multivalued = X_ov_lo < X_ov_hi

    delta_r2 = r2_dg - r2_sg if (r2_dg > -np.inf and r2_sg > -np.inf) else 0

    results[well] = {
        'n_peaks': n_peaks,
        'peak_times': peak_times,
        'Xmax': Xmax,
        'r2_single': r2_sg,
        'r2_double': r2_dg,
        'delta_r2': delta_r2,
        'sg_params': sg_params,
        'dg_params': dg_params,
        'xu': xu, 'Xp': Xp, 'Xu': Xu, 'raw': raw,
        'multivalued': multivalued,
        'condition': condition(well),
    }

    marker = '←BIPHASIC' if (n_peaks >= 2 and delta_r2 > 0.02) else ''
    print(f"  {well:3s} [{condition(well):22s}]: peaks={n_peaks}  "
          f"R²_1G={r2_sg:.3f}  R²_2G={r2_dg:.3f}  ΔR²={delta_r2:+.3f}  {marker}")

# ── Identify wells where 2-component significantly improves fit ───────────────
biphasic_wells = [w for w, r in results.items()
                  if r['delta_r2'] > 0.01 and r['r2_double'] > 0.90]
print(f"\nBiphasic wells (2G improves R² by >0.01 AND R²_2G>0.90): {biphasic_wells}")

# ── Figure 1: Overview of all wells ──────────────────────────────────────────
print("\nGenerating overview plot...")

n_wells = len(valid_wells)
ncols = 8
nrows = int(np.ceil(n_wells / ncols))
fig, axes = plt.subplots(nrows, ncols, figsize=(24, nrows*3))
axes = axes.flatten()

for i, well in enumerate(valid_wells):
    ax = axes[i]
    r = results[well]
    ax.plot(r['xu'], r['Xu'], 'k-', lw=1.5, alpha=0.8)

    # Single Gompertz
    if r['sg_params'] is not None:
        Xsg = G_analytic(r['xu'], r['sg_params'][0], r['sg_params'][1], r['sg_params'][2])
        ax.plot(r['xu'], Xsg, 'b--', lw=1.2, alpha=0.7)

    # Double Gompertz
    if r['dg_params'] is not None:
        r1, K1, lam1, r2, K2, lam2 = r['dg_params']
        X1 = G_analytic(r['xu'], K1, r1, lam1)
        X2 = G_analytic(r['xu'], K2, r2, lam2)
        ax.plot(r['xu'], X1+X2, 'r--', lw=1.5, alpha=0.9)
        ax.fill_between(r['xu'], 0, X1, alpha=0.1, color='red')
        ax.fill_between(r['xu'], X1, X1+X2, alpha=0.1, color='green')

    # Condition label
    cond = r['condition'].replace('Chloramphenicol_','CHL').replace('Rifampicin_','RIF')
    color = 'red' if r['delta_r2'] > 0.02 else ('blue' if r['r2_single'] > 0.95 else 'gray')
    ax.set_title(f"{well}\n{cond}\nR²1G={r['r2_single']:.2f} R²2G={r['r2_double']:.2f}",
                 fontsize=6.5, color=color)
    ax.set_xlabel('h', fontsize=6); ax.tick_params(labelsize=6)
    ax.grid(alpha=0.2)

for j in range(i+1, len(axes)):
    axes[j].set_visible(False)

fig.suptitle("LG166 — 2-Component Gompertz Discovery (red=2G, blue=1G, black=data)\n"
             "Red title = 2G improves R²>0.02", fontsize=11, fontweight='bold')
plt.tight_layout()
out1 = 'ode_inversion/LG166_overview.png'
plt.savefig(out1, dpi=120, bbox_inches='tight')
print(f"Saved: {out1}")
plt.close()

# ── Figure 2: Best wells deep dive ───────────────────────────────────────────
# Pick top 6 wells by R²_double and delta_r2
scored = sorted(results.items(), key=lambda x: x[1]['r2_double'], reverse=True)
top_wells = [w for w, r in scored if r['r2_double'] > 0.90][:8]

if not top_wells:
    top_wells = [w for w, r in scored[:6]]

print(f"\nTop wells for deep dive: {top_wells}")

n_top = len(top_wells)
fig2, axes2 = plt.subplots(2, min(n_top, 4), figsize=(18, 9))
if n_top < 4:
    axes2 = axes2.reshape(2, -1)
axes2 = axes2.flatten()

for i, well in enumerate(top_wells[:8]):
    ax = axes2[i]
    r = results[well]
    xu = r['xu']; Xu = r['Xu']

    # Raw data
    ax.plot(t_raw, r['raw'], '.', color='gray', ms=2, alpha=0.5, label='Raw data')
    ax.plot(xu, Xu, 'k-', lw=2, label='Smoothed')

    # Single Gompertz
    if r['sg_params'] is not None:
        Xsg = G_analytic(xu, r['sg_params'][0], r['sg_params'][1], r['sg_params'][2])
        ax.plot(xu, Xsg, 'b--', lw=2, label=f'1G  R²={r["r2_single"]:.3f}')

    # Double Gompertz
    if r['dg_params'] is not None:
        r1, K1, lam1, r2, K2, lam2 = r['dg_params']
        X1 = G_analytic(xu, K1, r1, lam1)
        X2 = G_analytic(xu, K2, r2, lam2)
        ax.plot(xu, X1+X2, 'r-', lw=2.5, label=f'2G  R²={r["r2_double"]:.3f}')
        ax.fill_between(xu, 0, X1, alpha=0.15, color='red', label=f'X₁ K={K1:.3f}')
        ax.fill_between(xu, X1, X1+X2, alpha=0.15, color='green', label=f'X₂ K={K2:.3f}')

    cond = r['condition'].replace('Chloramphenicol_','CHL').replace('Rifampicin_','RIF')
    ax.set_title(f"{well} — {cond}\nΔR²={r['delta_r2']:+.4f}", fontsize=9)
    ax.set_xlabel('Time [h]'); ax.set_ylabel('OD (bg subtracted)')
    ax.legend(fontsize=7, loc='lower right'); ax.grid(alpha=0.3)

for j in range(len(top_wells), len(axes2)):
    axes2[j].set_visible(False)

fig2.suptitle("LG166 — 2-Component Gompertz: Best Fits\n"
              "Red shading = X₁ (phase 1), Green = X₂ (phase 2)",
              fontsize=11, fontweight='bold')
plt.tight_layout()
out2 = 'ode_inversion/LG166_deepdive.png'
plt.savefig(out2, dpi=130, bbox_inches='tight')
print(f"Saved: {out2}")
plt.close()

# ── Figure 3: R² comparison across conditions ─────────────────────────────────
conds_unique = list(dict.fromkeys(results[w]['condition'] for w in valid_wells))
r2_by_cond_1g = {c: [] for c in conds_unique}
r2_by_cond_2g = {c: [] for c in conds_unique}
for well in valid_wells:
    r = results[well]
    c = r['condition']
    if r['r2_single'] > -np.inf: r2_by_cond_1g[c].append(r['r2_single'])
    if r['r2_double'] > -np.inf: r2_by_cond_2g[c].append(r['r2_double'])

fig3, (ax3a, ax3b) = plt.subplots(1, 2, figsize=(16, 6))
fig3.suptitle("LG166 — Model quality by condition", fontsize=12, fontweight='bold')

x_pos = np.arange(len(conds_unique))
w_bar = 0.35
means_1g = [np.mean(r2_by_cond_1g[c]) if r2_by_cond_1g[c] else 0 for c in conds_unique]
means_2g = [np.mean(r2_by_cond_2g[c]) if r2_by_cond_2g[c] else 0 for c in conds_unique]
stds_1g  = [np.std(r2_by_cond_1g[c])  if r2_by_cond_1g[c]  else 0 for c in conds_unique]
stds_2g  = [np.std(r2_by_cond_2g[c])  if r2_by_cond_2g[c]  else 0 for c in conds_unique]

ax3a.bar(x_pos - w_bar/2, means_1g, w_bar, yerr=stds_1g, label='1G', color='blue', alpha=0.7, capsize=4)
ax3a.bar(x_pos + w_bar/2, means_2g, w_bar, yerr=stds_2g, label='2G', color='red', alpha=0.7, capsize=4)
ax3a.axhline(0.95, color='green', ls='--', lw=1.5, label='R²=0.95')
ax3a.set_xticks(x_pos)
short_conds = [c.replace('Chloramphenicol_','CHL').replace('Rifampicin_','RIF').replace('control','ctrl') for c in conds_unique]
ax3a.set_xticklabels(short_conds, rotation=45, ha='right', fontsize=8)
ax3a.set_ylabel('Mean R²'); ax3a.set_title('R² by condition')
ax3a.legend(); ax3a.grid(alpha=0.3, axis='y')

# Delta R² (improvement from 2G)
deltas = [m2 - m1 for m1, m2 in zip(means_1g, means_2g)]
colors_bar = ['green' if d > 0.01 else ('gray' if d > 0 else 'red') for d in deltas]
ax3b.bar(x_pos, deltas, color=colors_bar, alpha=0.8, edgecolor='k', lw=0.8)
ax3b.axhline(0, color='k', lw=0.8)
ax3b.axhline(0.01, color='green', ls='--', lw=1.5, label='ΔR²=0.01 threshold')
ax3b.set_xticks(x_pos)
ax3b.set_xticklabels(short_conds, rotation=45, ha='right', fontsize=8)
ax3b.set_ylabel('ΔR² = R²_2G − R²_1G')
ax3b.set_title('Improvement from 2-component model\n(green = meaningful gain)')
ax3b.legend(); ax3b.grid(alpha=0.3, axis='y')
for xi, d in zip(x_pos, deltas):
    ax3b.text(xi, d + 0.001, f'{d:+.3f}', ha='center', va='bottom', fontsize=8)

plt.tight_layout()
out3 = 'ode_inversion/LG166_r2_comparison.png'
plt.savefig(out3, dpi=130, bbox_inches='tight')
print(f"Saved: {out3}")
plt.close()

# ── Summary table ─────────────────────────────────────────────────────────────
print("\n" + "="*70)
print("DISCOVERED 2-COMPONENT GOMPERTZ PARAMETERS (top wells)")
print("="*70)
print(f"{'Well':4s} {'Condition':22s} {'r1':6s} {'K1':6s} {'λ1':6s} {'r2':6s} {'K2':6s} {'λ2':6s} {'R²1G':6s} {'R²2G':6s}")
print("-"*70)
for well, r in sorted(results.items(), key=lambda x: x[1]['r2_double'], reverse=True)[:15]:
    if r['dg_params'] is not None:
        r1,K1,lam1,r2,K2,lam2 = r['dg_params']
        cond = r['condition'].replace('Chloramphenicol_','CHL').replace('Rifampicin_','RIF').replace('control','ctrl')
        print(f"{well:4s} {cond:22s} {r1:6.3f} {K1:6.3f} {lam1:6.1f} {r2:6.3f} {K2:6.3f} {lam2:6.1f} "
              f"{r['r2_single']:6.3f} {r['r2_double']:6.3f}")
print()
print(f"Wells with genuine biphasic improvement (ΔR²>0.01, R²_2G>0.90): {len(biphasic_wells)}")
print(f"  {biphasic_wells}")
print("\nDone.")
