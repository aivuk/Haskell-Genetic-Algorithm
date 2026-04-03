"""
Plot synthetic diauxic data WITH the ODE discovered by the pipeline.

Steps:
  1. Generate double-Gompertz diauxic data (long plateau)
  2. Apply ODE discovery methods:
       - Direct Gompertz scan   X' = r·X·ln(K/X)
       - Non-autonomous STRidge X' = F(X, t)
  3. Integrate discovered ODEs forward
  4. Plot: data vs each discovered model
"""

import signal
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp
from scipy.interpolate import UnivariateSpline
from scipy.ndimage import gaussian_filter1d
from scipy.optimize import curve_fit

# ── helpers ───────────────────────────────────────────────────────────────────

def rmse(y, yh):
    return float(np.sqrt(np.mean((np.array(y)-np.array(yh))**2)))

def r2(y, yh):
    y = np.array(y); yh = np.array(yh)
    return float(1 - np.sum((y-yh)**2) / np.sum((y-np.mean(y))**2))

def smooth_deriv(xs, ys, sigma_px=7, order=1):
    spl = UnivariateSpline(xs, ys, k=5, s=0)
    xu  = np.linspace(xs[0], xs[-1], max(len(xs), 800))
    yu  = spl(xu)
    dx  = xu[1] - xu[0]
    edge = min(int(np.ceil(3*sigma_px)), (len(xu)-20)//2)
    d   = gaussian_filter1d(yu, sigma_px, order=order, mode='nearest') / dx**order
    return xu[edge:-edge], d[edge:-edge], yu[edge:-edge]

def stridge(Theta, y, threshold=0.1, n_iter=20, ridge=1e-6):
    n = Theta.shape[1]
    active = list(range(n))
    c_full = np.zeros(n)
    for _ in range(n_iter):
        if not active: break
        Ta = Theta[:, active]
        c_a = np.linalg.lstsq(Ta.T@Ta + ridge*np.eye(len(active)), Ta.T@y, rcond=None)[0]
        mask = np.abs(c_a) >= threshold * np.max(np.abs(c_a))
        if not np.any(mask): break
        active = [active[i] for i in range(len(active)) if mask[i]]
        c_a = c_a[mask]
    if active:
        Ta = Theta[:, active]
        c_a = np.linalg.lstsq(Ta, y, rcond=None)[0]
        for i, idx in enumerate(active): c_full[idx] = c_a[i]
    return c_full

class _Timeout(Exception): pass
def _handler(s, f): raise _Timeout()

def safe_ivp(ode_func, t, y0, timeout=8):
    try:
        old = signal.signal(signal.SIGALRM, _handler)
        signal.alarm(timeout)
        sol = solve_ivp(ode_func, [t[0], t[-1]], y0, t_eval=t,
                        method='RK45', rtol=1e-5, atol=1e-7, max_step=1.0)
        signal.alarm(0); signal.signal(signal.SIGALRM, old)
        if sol.success and len(sol.y[0]) == len(t):
            return sol.y[0]
    except Exception:
        signal.alarm(0)
    return None


# ── Step 1: Generate data ─────────────────────────────────────────────────────

def G(t, A, mu, lam):
    return A * np.exp(-np.exp((mu*np.e/A)*(lam-t)+1))

def dG(t, A, mu, lam):
    z = (mu*np.e/A)*(lam-t)+1
    return mu*np.e * np.exp(-np.exp(z)+z)

A1, mu1, lam1 = 0.55, 0.50, 5.0
A2, mu2, lam2 = 0.55, 0.28, 48.0

rng = np.random.default_rng(42)
t   = np.linspace(0, 100, 600)
X   = G(t, A1, mu1, lam1) + G(t, A2, mu2, lam2) + rng.normal(0, 0.004, len(t))
X   = np.clip(X, 0, None)

print(f"Data: {len(t)} pts, X∈[{X.min():.3f},{X.max():.3f}]")

# Analytic derivative
Xp_true = dG(t, A1, mu1, lam1) + dG(t, A2, mu2, lam2)

# Smoothed derivative from data
xu, Xp, Xu = smooth_deriv(t, X, sigma_px=7, order=1)


# ── Step 2a: Direct Gompertz scan  X' = r·X·ln(K/X) ─────────────────────────

print("\n--- Method 1: Direct Gompertz scan ---")
Xc  = np.clip(Xu, 1e-6, None)
X_max = np.max(X)
best_g = {'res': np.inf}
for K in np.linspace(X_max*1.001, X_max*3, 150):
    phi = Xc * np.log(K / Xc)
    r   = np.dot(Xp, phi) / (np.dot(phi, phi) + 1e-10)
    res = rmse(Xp, r*phi) / (np.std(Xp)+1e-10)
    if res < best_g['res']:
        best_g = {'res': res, 'K': K, 'r': r}

K_g, r_g = best_g['K'], best_g['r']
print(f"  Discovered: X' = {r_g:.4f}·X·ln({K_g:.3f}/X)   (der_res={best_g['res']:.4f})")

def ode_gomp(tv, y): return [r_g * max(y[0],1e-9) * np.log(K_g/max(y[0],1e-9))]
X_gomp = safe_ivp(ode_gomp, t, [X[0]])
if X_gomp is not None:
    print(f"  Integration: R²={r2(X, X_gomp):.4f}, RMSE={rmse(X, X_gomp):.4f}")


# ── Step 2b: Parametric double-Gompertz fit (best possible) ──────────────────

print("\n--- Method 2: Parametric double-Gompertz (upper bound) ---")
def double_gomp_fit(t, A1, mu1, lam1, A2, mu2, lam2):
    return G(t, A1, mu1, lam1) + G(t, A2, mu2, lam2)

try:
    popt, _ = curve_fit(double_gomp_fit, t, X,
                        p0=[0.5, 0.4, 5, 0.5, 0.2, 45],
                        bounds=([0,0,-10, 0,0, 10],
                                [2, 3, 30, 2, 3, 100]),
                        maxfev=10000)
    X_dbl = double_gomp_fit(t, *popt)
    print(f"  Fit: A1={popt[0]:.3f} μ1={popt[1]:.3f} λ1={popt[2]:.1f}  "
          f"A2={popt[3]:.3f} μ2={popt[4]:.3f} λ2={popt[5]:.1f}")
    print(f"  R²={r2(X, X_dbl):.4f}, RMSE={rmse(X, X_dbl):.4f}")
except Exception as e:
    X_dbl = None
    print(f"  Failed: {e}")


# ── Step 2c: Non-autonomous STRidge  X' = F(X, t) ────────────────────────────

print("\n--- Method 3: Non-autonomous STRidge X' = F(X, t) ---")
alphas = [0.05, 0.10, 0.20, 0.50]

feat_names = []
feat_cols  = []
for alpha in alphas:
    feat_names.append(f'X·ln(K/X)·exp(-{alpha}t)')
    feat_cols.append(Xc * np.log(X_max*1.02/Xc) * np.exp(-alpha*xu))
# Also plain Gompertz term
feat_names.append('X·ln(K/X)')
feat_cols.append(Xc * np.log(X_max*1.02/Xc))

Theta = np.column_stack(feat_cols)
col_n = np.sqrt(np.sum(Theta**2, axis=0)) + 1e-10
Theta_n = Theta / col_n

best_na = {'r2': -np.inf}
for thr in [0.10, 0.20, 0.30, 0.50]:
    c_n = stridge(Theta_n, Xp, threshold=thr)
    c   = c_n / col_n
    active = [(feat_names[i], c[i]) for i in range(len(c)) if abs(c[i]) > 1e-8]
    if not active: continue

    # Integrate
    import re
    parsed = []
    for name, coef in active:
        if '·exp(-' in name:
            alpha = float(re.search(r'exp\(-([0-9.]+)t\)', name).group(1))
        else:
            alpha = 0.0
        parsed.append((alpha, coef))

    def ode_na(tv, y, parsed=parsed):
        Xv = max(y[0], 1e-9)
        dX = sum(c * Xv * np.log(X_max*1.02/Xv) * (np.exp(-a*tv) if a>0 else 1.0)
                 for a, c in parsed)
        return [dX]

    X_na = safe_ivp(ode_na, t, [X[0]])
    if X_na is not None:
        rv = r2(X, X_na)
        if rv > best_na['r2']:
            best_na = {'r2': rv, 'rmse': rmse(X, X_na),
                       'X_pred': X_na, 'active': active, 'thr': thr}

if best_na['r2'] > -np.inf:
    print(f"  Discovered (thr={best_na['thr']}):")
    for name, coef in best_na['active']:
        print(f"    {coef:+.4f}·{name}")
    print(f"  R²={best_na['r2']:.4f}, RMSE={best_na['rmse']:.4f}")
else:
    print("  No stable solution found")


# ── Step 3: MAIN PLOT — data + all discovered models ─────────────────────────

fig, axes = plt.subplots(2, 2, figsize=(15, 10))
fig.suptitle(
    "Diauxic Growth — Synthetic Data vs ODE Discovery Methods\n"
    "Data: Double-Gompertz (A₁=0.55,μ₁=0.50,λ₁=5 + A₂=0.55,μ₂=0.28,λ₂=48)  |  Plateau ≈ 39 h",
    fontsize=12, fontweight='bold')

colors = {'data':     ('k',  '-',  3.0),
          'gomp_scan':('r',  '--', 2.0),
          'dbl_fit':  ('b',  ':',  2.0),
          'na_stridg':('m',  '-.', 2.0)}

# (a) X(t): data + all models
ax = axes[0, 0]
ax.plot(t, X, 'k-', lw=2.5, label='Synthetic data', zorder=6)

if X_gomp is not None:
    r2g = r2(X, X_gomp)
    ax.plot(t, X_gomp, 'r--', lw=2,
            label=f"Discovered: X'=r·X·ln(K/X)  R²={r2g:.3f}", zorder=5)

if X_dbl is not None:
    r2d = r2(X, X_dbl)
    ax.plot(t, X_dbl, 'b:', lw=2,
            label=f"Double-Gompertz fit (upper bound)  R²={r2d:.3f}", zorder=4)

if best_na['r2'] > -np.inf:
    ax.plot(t, best_na['X_pred'], 'm-.', lw=2,
            label=f"Non-autonomous STRidge  R²={best_na['r2']:.3f}", zorder=3)

ax.set_xlabel('Time (h)', fontsize=11)
ax.set_ylabel('X(t)  [OD]', fontsize=11)
ax.set_title('(a)  X(t): data vs discovered ODEs', fontsize=11)
ax.legend(fontsize=9); ax.grid(alpha=0.3)

# (b) X'(t): true vs what each method captures
ax = axes[0, 1]
ax.plot(t, Xp_true, 'k-', lw=2.5, label="True X'(t) (analytic)", zorder=6)
ax.plot(xu, Xp, 'gray', lw=1.2, ls='-', alpha=0.5, label="X'(t) smoothed from data")

if X_gomp is not None:
    # Derivative of integrated Gompertz
    xu_g, Xp_g, _ = smooth_deriv(t, X_gomp, sigma_px=5, order=1)
    ax.plot(xu_g, Xp_g, 'r--', lw=2,
            label=f"Gompertz scan: X'=r·X·ln(K/X)")
    # Also plot ODE value directly
    phi_g = np.clip(t, None, None)
    Xp_gomp_ode = r_g * np.clip(X_gomp, 1e-9, None) * np.log(K_g / np.clip(X_gomp,1e-9,None))
    ax.plot(t, Xp_gomp_ode, 'r:', lw=1.5, alpha=0.6, label="Gompertz: r·X·ln(K/X) [ODE value]")

ax.axhline(0, color='gray', lw=0.5)
ax.set_xlabel('Time (h)', fontsize=11)
ax.set_ylabel("X'(t)", fontsize=11)
ax.set_title("(b)  Derivatives: true vs discovered", fontsize=11)
ax.legend(fontsize=8.5); ax.grid(alpha=0.3)

# (c) Residuals
ax = axes[1, 0]
if X_gomp is not None:
    ax.plot(t, X - X_gomp, 'r-', lw=1.8,
            label=f"Gompertz scan residual  (RMSE={rmse(X,X_gomp):.3f})")
if X_dbl is not None:
    ax.plot(t, X - X_dbl, 'b:', lw=1.8,
            label=f"Double-Gompertz fit  (RMSE={rmse(X,X_dbl):.3f})")
if best_na['r2'] > -np.inf:
    ax.plot(t, X - best_na['X_pred'], 'm-.', lw=1.8,
            label=f"Non-autonomous STRidge  (RMSE={best_na['rmse']:.3f})")
ax.axhline(0, color='k', lw=0.5)
ax.set_xlabel('Time (h)', fontsize=11)
ax.set_ylabel('Residual  X − X̂', fontsize=11)
ax.set_title('(c)  Residuals over time', fontsize=11)
ax.legend(fontsize=9); ax.grid(alpha=0.3)

# (d) R² bar chart + equation summary
ax = axes[1, 1]
models, r2s, clrs = [], [], []
if X_gomp is not None:
    models.append("Gompertz scan\n(model-free)")
    r2s.append(r2(X, X_gomp)); clrs.append('red')
if X_dbl is not None:
    models.append("Double-Gompertz\n(parametric, best)")
    r2s.append(r2(X, X_dbl)); clrs.append('blue')
if best_na['r2'] > -np.inf:
    models.append("Non-autonomous\nSTRidge")
    r2s.append(best_na['r2']); clrs.append('purple')

bars = ax.bar(models, r2s, color=clrs, alpha=0.75, edgecolor='k', width=0.5)
for bar, val in zip(bars, r2s):
    ax.text(bar.get_x()+bar.get_width()/2, val+0.01,
            f'R²={val:.3f}', ha='center', va='bottom', fontsize=10, fontweight='bold')
ax.set_ylim(0, 1.12)
ax.set_ylabel('R²', fontsize=11)
ax.set_title('(d)  Discovery method comparison', fontsize=11)
ax.axhline(1.0, color='gray', ls='--', lw=1, alpha=0.5)
ax.grid(alpha=0.3, axis='y')

# Add equation text
eq_text = (
    f"Discovered equations:\n\n"
    f"① Gompertz scan (model-free):\n"
    f"   X' = {r_g:.3f}·X·ln({K_g:.3f}/X)\n\n"
)
if best_na['r2'] > -np.inf and best_na['active']:
    eq_text += "② Non-autonomous STRidge:\n"
    for name, coef in best_na['active'][:3]:
        eq_text += f"   {coef:+.3f}·{name}\n"
eq_text += f"\n③ True model:\n   G₁(t) + G₂(t)\n   (double Gompertz)"
ax.text(0.02, 0.02, eq_text, transform=ax.transAxes,
        fontsize=8, verticalalignment='bottom',
        bbox=dict(boxstyle='round', facecolor='lightyellow', alpha=0.8))

plt.tight_layout()
fig.savefig('ode_inversion/diauxic_plateau_with_discovery.png', dpi=150, bbox_inches='tight')
print("\nSaved: ode_inversion/diauxic_plateau_with_discovery.png")


# ── Second plot: zoom on the plateau region showing model failure ─────────────

fig2, axes2 = plt.subplots(1, 2, figsize=(14, 5))
fig2.suptitle("Where the single-ODE model fails: the diauxic plateau region",
              fontsize=12, fontweight='bold')

# Focus on plateau region (roughly t=15 to t=60)
mask = (t >= 10) & (t <= 70)

ax = axes2[0]
ax.plot(t[mask], X[mask], 'k-', lw=2.5, label='Synthetic data', zorder=6)
if X_gomp is not None:
    ax.plot(t[mask], X_gomp[mask], 'r--', lw=2,
            label=f"Gompertz scan  R²={r2(X,X_gomp):.3f}")
if X_dbl is not None:
    ax.plot(t[mask], X_dbl[mask], 'b:', lw=2,
            label=f"Double-Gompertz fit  R²={r2(X,X_dbl):.3f}")
# Mark the plateau
ax.axvspan(15, 44, alpha=0.15, color='orange', label='Plateau region (~39h)')
ax.set_xlabel('Time (h)', fontsize=11)
ax.set_ylabel('X(t)  [OD]', fontsize=11)
ax.set_title('X(t) zoom — plateau region', fontsize=11)
ax.legend(fontsize=9); ax.grid(alpha=0.3)

ax = axes2[1]
xu_m = (xu >= 10) & (xu <= 70)
ax.plot(xu[xu_m], Xp[xu_m], 'k-', lw=2, label="Data X'(t)", zorder=6)
if X_gomp is not None:
    Xp_g_ode = r_g * np.clip(X_gomp, 1e-9, None) * np.log(K_g/np.clip(X_gomp,1e-9,None))
    ax.plot(t[mask], Xp_g_ode[mask], 'r--', lw=2,
            label="Gompertz scan: r·X·ln(K/X)")
ax.axhline(0, color='gray', lw=0.5)
ax.axvspan(15, 44, alpha=0.15, color='orange', label='Plateau X\'≈0')
ax.set_xlabel('Time (h)', fontsize=11)
ax.set_ylabel("X'(t)", fontsize=11)
ax.set_title("X'(t) zoom — why Gompertz fails here", fontsize=11)
ax.legend(fontsize=9); ax.grid(alpha=0.3)
ax.annotate("Single Gompertz predicts\nX'=0 only at X=K_max\nnot during plateau",
            xy=(30, 0.01), fontsize=9, color='darkred',
            bbox=dict(boxstyle='round', facecolor='mistyrose', alpha=0.8))

plt.tight_layout()
fig2.savefig('ode_inversion/diauxic_plateau_failure_zoom.png', dpi=150, bbox_inches='tight')
print("Saved: ode_inversion/diauxic_plateau_failure_zoom.png")
print("\nDone.")
