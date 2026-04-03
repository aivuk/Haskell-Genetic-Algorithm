#!/usr/bin/env python3
"""Generate 3D rigid body (Euler equations) data for Hamiltonian SR."""
import numpy as np, csv, os

# Moments of inertia
I = np.array([1.0, 2.0, 3.0])

def euler_deriv(L):
    """dL/dt = L x (I^{-1} L)  (Euler's equations)"""
    omega = L / I
    return np.cross(L, omega)

def rk4_step(L, dt):
    k1 = euler_deriv(L)
    k2 = euler_deriv(L + 0.5*dt*k1)
    k3 = euler_deriv(L + 0.5*dt*k2)
    k4 = euler_deriv(L + dt*k3)
    return L + dt*(k1 + 2*k2 + 2*k3 + k4)/6

np.random.seed(42)
rows = []
for _ in range(50):
    # Random initial angular momentum on unit sphere, scaled
    L = np.random.randn(3)
    L = L / np.linalg.norm(L) * np.random.uniform(0.5, 2.0)
    for step in range(60):
        dL = euler_deriv(L)
        rows.append([L[0], L[1], L[2], dL[0], dL[1], dL[2]])
        L = rk4_step(L, 0.02)

out = os.path.join(os.path.dirname(__file__), '..', 'data', 'rigidbody.csv')
os.makedirs(os.path.dirname(out), exist_ok=True)
with open(out, 'w', newline='') as f:
    w = csv.writer(f)
    w.writerow(['lx','ly','lz','dlx_dt','dly_dt','dlz_dt'])
    w.writerows(rows)
print(f"Wrote {len(rows)} rows to {out}")
