"""
Generate synthetic diauxic growth data with a CLEARLY VISIBLE long plateau phase.

Two approaches:
  A) Double-Gompertz (phenomenological) — guaranteed two-phase shape
  B) Mechanistic Monod with repressor + sharp induction switch
"""

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp
from scipy.interpolate import UnivariateSpline
from scipy.ndimage import gaussian_filter1d


# ── Approach A: Double-Gompertz ───────────────────────────────────────────────

def G(t, A, mu, lam):
    """Gompertz growth curve."""
    return A * np.exp(-np.exp((mu * np.e / A) * (lam - t) + 1))

def dG(t, A, mu, lam):
    """Analytical derivative of Gompertz."""
    z = (mu * np.e / A) * (lam - t) + 1
    return A * mu * np.e / A * np.exp(-np.exp(z) + z)

def double_gompertz(t_end=90, n_pts=500,
                    A1=0.55, mu1=0.50, lam1=5.0,
                    A2=0.55, mu2=0.30, lam2=40.0,
                    noise_std=0.005):
    """
    Two sequential Gompertz curves with a gap (plateau) between them.
    Plateau duration ≈ lam2 - lam1 - (saturation_time_1) hours.
    """
    t = np.linspace(0, t_end, n_pts)
    X = G(t, A1, mu1, lam1) + G(t, A2, mu2, lam2)
    if noise_std > 0:
        rng = np.random.default_rng(42)
        X = X + rng.normal(0, noise_std, len(t))
    return t, X


# ── Approach B: Mechanistic — Monod + repressor + bistable switch ─────────────

def generate_monod_bistable(
        mu1=0.90, mu2=0.70,
        Ks=0.005, Kd=0.003, n_hill=40,  # very sharp Hill
        tau_lag=10.0,                    # lag = fixed delay after S1 depletes
        X0=0.01, S10=0.40, S20=0.60,
        t_end=90, n_pts=600):
    """
    Monod diauxic with a bistable lag switch.
    The 'enzyme' E has TWO stable states (0 and 1) separated by a Hill.
    A positive-feedback loop makes E stay at 0 until triggered by S1 depletion,
    then jumps to 1 after ~tau_lag hours.

    Switch ODE:
      dE/dt = (1 - H(S1)) * E^n_sw / (E^n_sw + K_sw^n_sw) * k_on
              - H(S1) * k_off * E
    Bistable: k_on large, K_sw=0.5, n_sw=4
    """
    n_sw = 6; K_sw = 0.3
    k_on  = 1.0 / tau_lag * 10   # speed of switch-on
    k_off = 5.0                   # fast repression

    def hill_S(S1):
        return S1**n_hill / (S1**n_hill + Kd**n_hill)

    def hill_E(E):
        return E**n_sw / (E**n_sw + K_sw**n_sw)

    def odes(t, y):
        X, S1, S2, E = y
        X  = max(X, 0); S1 = max(S1, 0); S2 = max(S2, 0)
        E  = float(np.clip(E, 0, 1))

        HS1 = hill_S(S1)
        HE  = hill_E(E)
        r1  = mu1 * S1 / (S1 + Ks)
        r2  = mu2 * S2 / (S2 + Ks)

        dX  = (r1 * HS1 + r2 * E * (1 - HS1)) * X
        dS1 = -r1 * HS1 * X
        dS2 = -r2 * E * (1 - HS1) * X
        # Bistable switch: off when S1 present, switches on after S1 gone
        dE  = (1 - HS1) * HE * k_on * (1 - E) - HS1 * k_off * E
        return [dX, dS1, dS2, dE]

    # Small perturbation to get bistable switch started
    E0 = 0.01
    t_eval = np.linspace(0, t_end, n_pts)
    sol = solve_ivp(odes, [0, t_end], [X0, S10, S20, E0],
                    t_eval=t_eval, method='RK45',
                    rtol=1e-9, atol=1e-11, max_step=0.02)
    return sol.t, sol.y[0], sol.y[1], sol.y[2], sol.y[3]


def smooth_deriv(xs, ys, sigma_px=7, order=1):
    spl = UnivariateSpline(xs, ys, k=5, s=0)
    xu  = np.linspace(xs[0], xs[-1], max(len(xs), 800))
    yu  = spl(xu)
    dx  = xu[1] - xu[0]
    edge = min(int(np.ceil(3*sigma_px)), (len(xu)-20)//2)
    d   = gaussian_filter1d(yu, sigma_px, order=order, mode='nearest') / dx**order
    return xu[edge:-edge], d[edge:-edge], yu[edge:-edge]


def find_peaks(xu, Xp, min_height_frac=0.06, min_sep=4.0):
    threshold = min_height_frac * np.max(Xp)
    raw = []
    for i in range(1, len(Xp)-1):
        if Xp[i] > Xp[i-1] and Xp[i] > Xp[i+1] and Xp[i] > threshold:
            raw.append((xu[i], Xp[i]))
    merged = []
    for pk in raw:
        if merged and abs(pk[0]-merged[-1][0]) < min_sep:
            if pk[1] > merged[-1][1]: merged[-1] = pk
        else:
            merged.append(pk)
    return merged


def plot_diauxic(t, X, S1=None, S2=None, E=None,
                 Xp_analytic=None, title='', outfile='', noise_std=0):

    xu, Xp, Xu = smooth_deriv(t, X, sigma_px=7, order=1)
    peaks = find_peaks(xu, Xp)

    # Plateau: trough between first two peaks
    plateau_start = plateau_end = plateau_dur = None
    if len(peaks) >= 2:
        t1, t2 = peaks[0][0], peaks[1][0]
        mask = (xu >= t1) & (xu <= t2)
        Xp_sub = Xp[mask]
        xu_sub = xu[mask]
        trough_val = np.min(Xp_sub)
        trough_t   = xu_sub[np.argmin(Xp_sub)]
        # Plateau = region where X'(t) < 25% of the minimum of the two peaks
        threshold_p = 0.25 * min(peaks[0][1], peaks[1][1])
        flat_mask = Xp_sub < threshold_p
        if np.any(flat_mask):
            plateau_start = xu_sub[flat_mask][0]
            plateau_end   = xu_sub[flat_mask][-1]
            plateau_dur   = plateau_end - plateau_start

    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    fig.suptitle(title, fontsize=13, fontweight='bold')

    # (a) X(t)
    ax = axes[0, 0]
    ax.plot(t, X, 'k-', lw=2.5, label='X(t)  [observable]', zorder=5)
    if S1 is not None:
        ax.plot(t, S1 * (X.max()/S1.max()) if S1.max()>0 else S1,
                'g--', lw=1.5, alpha=0.7, label='S1 (scaled)')
    if S2 is not None:
        ax.plot(t, S2 * (X.max()/S2.max()) if S2.max()>0 else S2,
                'm--', lw=1.5, alpha=0.7, label='S2 (scaled)')
    if plateau_start is not None:
        ax.axvspan(plateau_start, plateau_end, alpha=0.18, color='orange')
        ax.annotate('', xy=(plateau_end, X.max()*0.56),
                    xytext=(plateau_start, X.max()*0.56),
                    arrowprops=dict(arrowstyle='<->', color='darkorange', lw=2))
        ax.text((plateau_start+plateau_end)/2, X.max()*0.62,
                f'Plateau  ≈ {plateau_dur:.0f} h',
                ha='center', fontsize=10, color='darkorange', fontweight='bold')
    ax.set_xlabel('Time (h)'); ax.set_ylabel('X(t) [OD]')
    ax.set_title('(a)  Biomass X(t)', fontsize=11)
    ax.legend(fontsize=9); ax.grid(alpha=0.3)

    # (b) X'(t) fingerprint
    ax = axes[0, 1]
    ax.plot(xu, Xp, 'k-', lw=2.5, label="X'(t) smoothed")
    if Xp_analytic is not None:
        ax.plot(t, Xp_analytic, 'r--', lw=1.5, alpha=0.7, label="X'(t) analytic")
    ax.fill_between(xu, Xp, 0, where=Xp > 0, alpha=0.15, color='green')
    ax.axhline(0, color='gray', lw=0.5)
    for i, (tp, vp) in enumerate(peaks[:3]):
        ax.axvline(tp, color='r', ls='--', lw=1.5, alpha=0.8)
        ax.annotate(f'Peak {i+1}\nt={tp:.1f}h\nX\'={vp:.3f}',
                    xy=(tp, vp), xytext=(tp+2.5, vp*0.85),
                    fontsize=8.5, color='darkred',
                    arrowprops=dict(arrowstyle='->', color='darkred'))
    if plateau_start is not None:
        ax.axvspan(plateau_start, plateau_end, alpha=0.18, color='orange')
        ax.text((plateau_start+plateau_end)/2, 0.002,
                f"X'≈0  ({plateau_dur:.0f}h)",
                ha='center', fontsize=9, color='darkorange', fontweight='bold')
    ax.set_xlabel('Time (h)'); ax.set_ylabel("X'(t)")
    ax.set_title(f"(b)  Derivative fingerprint  —  {len(peaks)} peaks", fontsize=11)
    ax.legend(fontsize=9); ax.grid(alpha=0.3)

    # (c) hidden state or zoom on plateau
    ax = axes[1, 0]
    if E is not None:
        ax2c = ax.twinx()
        ax.plot(t, S1, 'g-', lw=2, label='S1')
        ax.plot(t, S2, 'm-', lw=2, label='S2')
        ax2c.plot(t, E, 'b--', lw=2, alpha=0.85, label='Enzyme E')
        ax.set_ylabel('Substrate conc.'); ax2c.set_ylabel('E  [0–1]', color='b')
        ax2c.tick_params(axis='y', colors='b')
        l1, lab1 = ax.get_legend_handles_labels()
        l2, lab2 = ax2c.get_legend_handles_labels()
        ax.legend(l1+l2, lab1+lab2, fontsize=9)
        ax.set_title('(c)  Hidden state', fontsize=11)
        if plateau_start is not None:
            ax.axvspan(plateau_start, plateau_end, alpha=0.12, color='orange')
    else:
        # Zoom on plateau region for double-Gompertz
        if plateau_start is not None:
            pad = (plateau_end - plateau_start) * 0.5
            mask_z = (t >= plateau_start-pad) & (t <= plateau_end+pad)
            ax.plot(t[mask_z], X[mask_z], 'k-', lw=2.5)
            ax.axvspan(plateau_start, plateau_end, alpha=0.25, color='orange')
            ax.set_title('(c)  Zoom on plateau region', fontsize=11)
        else:
            ax.plot(t, X, 'k-', lw=2.5)
            ax.set_title('(c)  X(t)', fontsize=11)
        ax.set_xlabel('Time (h)'); ax.set_ylabel('X(t)')
    ax.grid(alpha=0.3); ax.set_xlabel('Time (h)')

    # (d) phase portrait X' vs X (coloured by time)
    ax = axes[1, 1]
    t_col = xu / xu[-1]
    sc = ax.scatter(Xu, Xp, c=t_col, cmap='plasma', s=6, alpha=0.75)
    plt.colorbar(sc, ax=ax, label='Normalised time')
    if plateau_start is not None:
        mp = (xu >= plateau_start) & (xu <= plateau_end)
        if np.any(mp):
            ax.scatter(Xu[mp], Xp[mp], c='orange', s=35,
                       zorder=5, edgecolors='k', lw=0.3, label='Plateau')
    ax.axhline(0, color='gray', lw=0.5)
    ax.set_xlabel('X'); ax.set_ylabel("X'")
    ax.set_title("(d)  Phase portrait  (coloured by time)", fontsize=11)
    ax.legend(fontsize=9); ax.grid(alpha=0.3)

    plt.tight_layout()
    fig.savefig(outfile, dpi=150, bbox_inches='tight')
    plt.close(fig)
    print(f"Saved: {outfile}  |  {len(peaks)} peaks  |  "
          f"plateau≈{plateau_dur:.0f}h" if plateau_dur else f"plateau not found")
    return peaks, plateau_dur


# ── Run both approaches ───────────────────────────────────────────────────────

print("=== Approach A: Double-Gompertz (phenomenological) ===")

for label, kw in [
    ('short_plateau',  dict(lam2=25, A1=0.55, mu1=0.50, lam1=5, A2=0.55, mu2=0.30, t_end=70)),
    ('medium_plateau', dict(lam2=35, A1=0.55, mu1=0.50, lam1=5, A2=0.55, mu2=0.30, t_end=85)),
    ('long_plateau',   dict(lam2=50, A1=0.55, mu1=0.50, lam1=5, A2=0.55, mu2=0.30, t_end=105)),
]:
    t, X = double_gompertz(**kw, noise_std=0.003)
    Xp_an = dG(t, kw['A1'], kw['mu1'], kw['lam1']) + dG(t, kw['A2'], kw['mu2'], kw['lam2'])
    out = f"ode_inversion/diauxic_A_{label}.png"
    plot_diauxic(t, X, Xp_analytic=Xp_an,
                 title=f"Double-Gompertz — {label.replace('_', ' ')}",
                 outfile=out)


print("\n=== Approach B: Mechanistic bistable switch ===")

for tau in [6, 10, 16]:
    t, X, S1, S2, E = generate_monod_bistable(tau_lag=tau, t_end=90+tau*2)
    out = f"ode_inversion/diauxic_B_tau{tau}.png"
    plot_diauxic(t, X, S1=S1, S2=S2, E=E,
                 title=f"Mechanistic bistable switch — τ_lag={tau}h",
                 outfile=out)


# ── Best scenario: long-plateau double-Gompertz, high quality ─────────────────

print("\n=== Best scenario: Double-Gompertz long plateau ===")
t, X = double_gompertz(
    A1=0.55, mu1=0.50, lam1=5.0,
    A2=0.55, mu2=0.28, lam2=48.0,
    t_end=100, n_pts=600, noise_std=0.004
)
Xp_an = dG(t, 0.55, 0.50, 5.0) + dG(t, 0.55, 0.28, 48.0)
xu, Xp, Xu = smooth_deriv(t, X, sigma_px=7, order=1)
peaks = find_peaks(xu, Xp)

plateau_start = plateau_end = plateau_dur = None
if len(peaks) >= 2:
    t1, t2 = peaks[0][0], peaks[1][0]
    mask = (xu >= t1) & (xu <= t2)
    Xp_sub = Xp[mask]; xu_sub = xu[mask]
    thr = 0.20 * min(peaks[0][1], peaks[1][1])
    flat = Xp_sub < thr
    if np.any(flat):
        plateau_start = xu_sub[flat][0]
        plateau_end   = xu_sub[flat][-1]
        plateau_dur   = plateau_end - plateau_start

print(f"Peaks: {len(peaks)}, plateau≈{plateau_dur:.0f}h, Xmax={X.max():.3f}")

fig, axes = plt.subplots(2, 2, figsize=(14, 10))
fig.suptitle("Diauxic Growth — Double-Gompertz Model (Long Plateau)\n"
             "Observable: X(t) only  |  A₁=0.55, μ₁=0.50, λ₁=5  +  A₂=0.55, μ₂=0.28, λ₂=48",
             fontsize=12, fontweight='bold')

ax = axes[0, 0]
ax.plot(t, X, 'k-', lw=3, label='X(t)  [observable + noise]', zorder=5)
ax.plot(t, G(t, 0.55, 0.50, 5.0), 'g--', lw=1.5, alpha=0.8, label='Phase 1  G₁(t)')
ax.plot(t, G(t, 0.55, 0.28, 48.0), 'm--', lw=1.5, alpha=0.8, label='Phase 2  G₂(t)')
if plateau_start is not None:
    ax.axvspan(plateau_start, plateau_end, alpha=0.18, color='orange')
    ax.annotate('', xy=(plateau_end, X.max()*0.57),
                xytext=(plateau_start, X.max()*0.57),
                arrowprops=dict(arrowstyle='<->', color='darkorange', lw=2.5))
    ax.text((plateau_start+plateau_end)/2, X.max()*0.64,
            f'Diauxic plateau  ≈ {plateau_dur:.0f} h',
            ha='center', fontsize=11, color='darkorange', fontweight='bold')
ax.set_xlabel('Time (h)', fontsize=11)
ax.set_ylabel('X(t)  [OD]', fontsize=11)
ax.set_title('(a)  Biomass X(t)', fontsize=11)
ax.legend(fontsize=9); ax.grid(alpha=0.3)

ax = axes[0, 1]
ax.plot(xu, Xp, 'k-', lw=2.5, label="X'(t) smoothed")
ax.plot(t, Xp_an, 'r--', lw=1.8, alpha=0.75, label="X'(t) analytic (no noise)")
ax.fill_between(xu, Xp, 0, where=Xp > 0, alpha=0.15, color='green')
ax.axhline(0, color='gray', lw=0.5)
for i, (tp, vp) in enumerate(peaks[:2]):
    ax.axvline(tp, color='r', ls='--', lw=1.5, alpha=0.8)
    ax.annotate(f'Peak {i+1}\nt={tp:.1f}h', xy=(tp, vp),
                xytext=(tp+3, vp*0.88), fontsize=9.5, color='darkred',
                arrowprops=dict(arrowstyle='->', color='darkred'))
if plateau_start is not None:
    ax.axvspan(plateau_start, plateau_end, alpha=0.18, color='orange')
    ax.text((plateau_start+plateau_end)/2, 0.002,
            f"X'≈0  ({plateau_dur:.0f}h)",
            ha='center', fontsize=10, color='darkorange', fontweight='bold')
ax.set_xlabel('Time (h)', fontsize=11)
ax.set_ylabel("X'(t)", fontsize=11)
ax.set_title(f"(b)  Derivative fingerprint  —  {len(peaks)} peaks", fontsize=11)
ax.legend(fontsize=9); ax.grid(alpha=0.3)

ax = axes[1, 0]
# Zoom on X(t) near plateau
if plateau_start is not None:
    pad = 10
    mz = (t >= plateau_start-pad) & (t <= plateau_end+pad)
    ax.plot(t[mz], X[mz], 'k-', lw=3, label='X(t)')
    ax.plot(t[mz], G(t[mz], 0.55, 0.50, 5.0), 'g--', lw=1.5, alpha=0.8, label='G₁(t)')
    ax.plot(t[mz], G(t[mz], 0.55, 0.28, 48.0), 'm--', lw=1.5, alpha=0.8, label='G₂(t)')
    ax.axvspan(plateau_start, plateau_end, alpha=0.25, color='orange',
               label=f'Plateau ({plateau_dur:.0f}h)')
    ax.set_xlim(plateau_start-pad, plateau_end+pad)
ax.set_xlabel('Time (h)', fontsize=11); ax.set_ylabel('X(t)', fontsize=11)
ax.set_title('(c)  Zoom: plateau region', fontsize=11)
ax.legend(fontsize=9); ax.grid(alpha=0.3)

ax = axes[1, 1]
t_col = xu / xu[-1]
sc = ax.scatter(Xu, Xp, c=t_col, cmap='plasma', s=8, alpha=0.8)
plt.colorbar(sc, ax=ax, label='Time (normalised)')
if plateau_start is not None:
    mp = (xu >= plateau_start) & (xu <= plateau_end)
    if np.any(mp):
        ax.scatter(Xu[mp], Xp[mp], c='orange', s=45, zorder=5,
                   edgecolors='k', lw=0.4, label='Plateau')
ax.axhline(0, color='gray', lw=0.5)
ax.set_xlabel('X', fontsize=11); ax.set_ylabel("X'", fontsize=11)
ax.set_title("(d)  Phase portrait  X' vs X", fontsize=11)
ax.legend(fontsize=9); ax.grid(alpha=0.3)

plt.tight_layout()
fig.savefig('ode_inversion/diauxic_long_plateau.png', dpi=150, bbox_inches='tight')
print("Saved: ode_inversion/diauxic_long_plateau.png")
print("Done.")
