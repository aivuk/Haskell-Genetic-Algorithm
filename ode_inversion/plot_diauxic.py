"""
Plot synthetic diauxic growth data vs discovered ODE.
"""

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp
from scipy.interpolate import UnivariateSpline
from scipy.ndimage import gaussian_filter1d
from scipy.optimize import curve_fit

# ── Reproduce data ────────────────────────────────────────────────────────────

def generate_diauxic_monod(mu1=0.90, mu2=0.25, Ks1=0.01, Ks2=0.01,
                            Kd=0.005, n_hill=10, Y1=1.0, Y2=1.0,
                            X0=0.01, S10=0.50, S20=1.00, t_end=50, n_pts=300):
    def hill(S1):
        return S1**n_hill / (S1**n_hill + Kd**n_hill)
    def odes(t, y):
        X, S1, S2 = y
        X = max(X, 0); S1 = max(S1, 0); S2 = max(S2, 0)
        H = hill(S1)
        mu_eff = mu1*S1/(S1+Ks1)*H + mu2*S2/(S2+Ks2)*(1-H)
        dX  = mu_eff * X
        dS1 = -mu1*S1/(S1+Ks1)*H*X/Y1
        dS2 = -mu2*S2/(S2+Ks2)*(1-H)*X/Y2
        return [dX, dS1, dS2]
    t_eval = np.linspace(0, t_end, n_pts)
    sol = solve_ivp(odes, [0, t_end], [X0, S10, S20],
                    t_eval=t_eval, method='RK45', rtol=1e-8, atol=1e-10, max_step=0.1)
    return sol.t, sol.y[0], sol.y[1], sol.y[2]

t, X, S1, S2 = generate_diauxic_monod()

# ── Smooth derivative ─────────────────────────────────────────────────────────

def smooth_deriv(xs, ys, sigma_px=8, order=1):
    spl = UnivariateSpline(xs, ys, k=5, s=0)
    xu = np.linspace(xs[0], xs[-1], max(len(xs), 400))
    yu = spl(xu)
    dx = xu[1] - xu[0]
    edge = min(int(np.ceil(3*sigma_px)), (len(xu)-20)//2)
    d = gaussian_filter1d(yu, sigma_px, order=order, mode='nearest') / dx**order
    return xu[edge:-edge], d[edge:-edge], yu[edge:-edge]

xu, Xp, Xu = smooth_deriv(t, X, sigma_px=8, order=1)

# ── Discovered ODE: X' = r * X * ln(K/X) ─────────────────────────────────────
r_disc, K_disc = 0.3949, 1.5403

def gompertz_ode(t_val, y):
    Xv = max(y[0], 1e-10)
    return [r_disc * Xv * np.log(K_disc / Xv)]

sol_disc = solve_ivp(gompertz_ode, [t[0], t[-1]], [X[0]],
                     t_eval=t, method='RK45', rtol=1e-6, atol=1e-8, max_step=0.5)
X_disc = sol_disc.y[0]

# ── Parametric Gompertz fit ───────────────────────────────────────────────────

def gompertz(t, A, mu, lam):
    return A * np.exp(-np.exp((mu * np.e / A) * (lam - t) + 1))

popt, _ = curve_fit(gompertz, t, X,
                    p0=[1.55, 0.3, 3.0],
                    bounds=([0, 0, -50], [10, 5, 50]), maxfev=5000)
X_gomp = gompertz(t, *popt)

# ── R² ───────────────────────────────────────────────────────────────────────

def r2(y, yh):
    y = np.array(y); yh = np.array(yh)
    return 1 - np.sum((y-yh)**2) / np.sum((y-np.mean(y))**2)

r2_disc = r2(X, X_disc)
r2_gomp = r2(X, X_gomp)

# ── Peaks in X'(t) ───────────────────────────────────────────────────────────
peaks = []
for i in range(1, len(Xp)-1):
    if Xp[i] > Xp[i-1] and Xp[i] > Xp[i+1] and Xp[i] > 0.001*np.max(Xp):
        peaks.append((xu[i], Xp[i]))
merged = []
for pk in peaks:
    if merged and abs(pk[0]-merged[-1][0]) < 2.0:
        if pk[1] > merged[-1][1]: merged[-1] = pk
    else:
        merged.append(pk)

# ── Plotting ──────────────────────────────────────────────────────────────────

fig, axes = plt.subplots(2, 2, figsize=(13, 9))
fig.suptitle("Synthetic Diauxic Growth — Model-Free ODE Discovery", fontsize=13, fontweight='bold')

# Panel 1: X(t) with fits
ax = axes[0, 0]
ax.plot(t, X, 'k-', lw=2, label='Synthetic data (Monod 2-substrate)')
ax.plot(t, X_disc, 'r--', lw=2,
        label=f"Discovered: X'=r·X·ln(K/X)  (R²={r2_disc:.4f})")
ax.plot(t, X_gomp, 'b:', lw=2,
        label=f"Gompertz parametric fit  (R²={r2_gomp:.4f})")
ax.set_xlabel('Time'); ax.set_ylabel('Biomass X(t) [OD]')
ax.set_title('(a) Biomass X(t) — data vs discovered ODE')
ax.legend(fontsize=8); ax.grid(alpha=0.3)

# Panel 2: Derivative X'(t) — diauxic fingerprint
ax = axes[0, 1]
ax.plot(xu, Xp, 'k-', lw=2, label="X'(t) (smoothed)")
for i, (tp, vp) in enumerate(merged):
    ax.axvline(tp, color='r', ls='--', alpha=0.7)
    ax.annotate(f'Peak {i+1}\nt={tp:.1f}', xy=(tp, vp),
                xytext=(tp+1.5, vp*0.98), fontsize=8, color='red')
ax.axhline(0, color='gray', lw=0.5)
ax.set_xlabel('Time'); ax.set_ylabel("X'(t)")
ax.set_title(f'(b) Derivative fingerprint — {len(merged)} peaks (diauxic signature)')
ax.legend(fontsize=8); ax.grid(alpha=0.3)

# Panel 3: Substrates S1, S2 (hidden state)
ax = axes[1, 0]
ax.plot(t, S1, 'g-', lw=2, label='S1 (1st substrate)')
ax.plot(t, S2, 'm-', lw=2, label='S2 (2nd substrate)')
ax.plot(t, X,  'k--', lw=1.5, alpha=0.6, label='X (biomass, scale right)')
ax.set_xlabel('Time'); ax.set_ylabel('Concentration')
ax.set_title('(c) Hidden state: substrate dynamics')
ax.legend(fontsize=8); ax.grid(alpha=0.3)
ax2 = ax.twinx()
ax2.set_ylabel('X(t)')
# Just for reference frame, don't re-plot

# Panel 4: ODE phase portrait X' vs X
ax = axes[1, 1]
spl_X = UnivariateSpline(t, X, k=5, s=0)
X_fine = np.linspace(0.02, X.max()*1.02, 300)

# Discovered ODE curve: X' = r * X * ln(K/X)
Xp_disc_curve = r_disc * X_fine * np.log(K_disc / X_fine)

# Compute data X' at X values (using smoothed derivative)
Xp_data = smooth_deriv(t, X, sigma_px=8, order=1)
xu_p, Xp_arr, Xu_arr = Xp_data

ax.plot(Xu_arr, Xp_arr, 'k.', ms=3, alpha=0.5, label='Data trajectory (X, X\')')
ax.plot(X_fine, Xp_disc_curve, 'r-', lw=2,
        label=f"Discovered: r·X·ln(K/X)\nr={r_disc:.3f}, K={K_disc:.3f}")
ax.axhline(0, color='gray', lw=0.5)
ax.axvline(K_disc, color='r', ls=':', alpha=0.5, label=f'K={K_disc:.3f}')
ax.set_xlabel('X'); ax.set_ylabel("X'")
ax.set_title("(d) Phase portrait: X' vs X")
ax.legend(fontsize=8); ax.grid(alpha=0.3)

plt.tight_layout()
out = 'ode_inversion/diauxic_discovery_plot.png'
plt.savefig(out, dpi=150, bbox_inches='tight')
print(f"Saved: {out}")

# ── Second figure: residuals and ODE discovery quality ────────────────────────

fig2, axes2 = plt.subplots(1, 3, figsize=(14, 4))
fig2.suptitle("ODE Discovery Quality", fontsize=12, fontweight='bold')

ax = axes2[0]
ax.plot(t, X - X_disc, 'r-', lw=1.5, label=f'Discovered ODE residual')
ax.plot(t, X - X_gomp, 'b--', lw=1.5, label='Gompertz parametric residual')
ax.axhline(0, color='k', lw=0.5)
ax.set_xlabel('Time'); ax.set_ylabel('Residual X - X_pred')
ax.set_title('Residuals')
ax.legend(fontsize=9); ax.grid(alpha=0.3)

ax = axes2[1]
K_vals = np.linspace(X.max()*1.001, X.max()*3.0, 100)
xu_scan, Xp_scan, X_scan = smooth_deriv(t, X, sigma_px=10, order=1)
Xc_scan = np.clip(X_scan, 1e-6, None)
residuals_scan = []
for K in K_vals:
    phi = Xc_scan * np.log(K / Xc_scan)
    r = np.dot(Xp_scan, phi) / (np.dot(phi, phi) + 1e-10)
    res = np.sqrt(np.mean((Xp_scan - r*phi)**2)) / (np.std(Xp_scan) + 1e-10)
    residuals_scan.append(res)
ax.plot(K_vals, residuals_scan, 'g-', lw=2)
ax.axvline(K_disc, color='r', ls='--', label=f'Best K={K_disc:.3f}')
ax.set_xlabel('K (scanned)'); ax.set_ylabel('Normalised residual')
ax.set_title('K-scan for Gompertz ODE discovery')
ax.legend(fontsize=9); ax.grid(alpha=0.3)

ax = axes2[2]
models = ['Discovered\n(model-free)', 'Gompertz\n(parametric)', 'aHPM\n(autonomous)']
r2s = [r2_disc, r2_gomp, -0.865]
colors = ['red', 'blue', 'gray']
bars = ax.bar(models, r2s, color=colors, alpha=0.7, edgecolor='k')
ax.axhline(0, color='k', lw=0.5)
ax.set_ylabel('R²')
ax.set_title('Model comparison')
for bar, val in zip(bars, r2s):
    ax.text(bar.get_x() + bar.get_width()/2, max(val+0.02, 0.05),
            f'{val:.3f}', ha='center', va='bottom', fontsize=10, fontweight='bold')
ax.set_ylim(-1.2, 1.1)
ax.grid(alpha=0.3, axis='y')

plt.tight_layout()
out2 = 'ode_inversion/diauxic_discovery_quality.png'
plt.savefig(out2, dpi=150, bbox_inches='tight')
print(f"Saved: {out2}")
print("Done.")
