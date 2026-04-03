#!/usr/bin/env python3
import numpy as np, csv, sys, os

def rk4(q, p, k, dt):
    def deriv(q, p):
        return p, -k*q
    k1q, k1p = deriv(q, p)
    k2q, k2p = deriv(q + 0.5*dt*k1q, p + 0.5*dt*k1p)
    k3q, k3p = deriv(q + 0.5*dt*k2q, p + 0.5*dt*k2p)
    k4q, k4p = deriv(q + dt*k3q, p + dt*k3p)
    dq = dt*(k1q + 2*k2q + 2*k3q + k4q)/6
    dp = dt*(k1p + 2*k2p + 2*k3p + k4p)/6
    return q+dq, p+dp

np.random.seed(42)
rows = []
for _ in range(50):
    k = np.random.uniform(3.8, 4.2)
    q, p = np.random.uniform(-2, 2, 2)
    for step in range(60):
        dq_dt = p
        dp_dt = -k*q
        rows.append([q, p, dq_dt, dp_dt])
        q, p = rk4(q, p, k, 0.05)

out = os.path.join(os.path.dirname(__file__), '..', 'data', 'harmonic.csv')
os.makedirs(os.path.dirname(out), exist_ok=True)
with open(out, 'w', newline='') as f:
    w = csv.writer(f)
    w.writerow(['q','p','dq_dt','dp_dt'])
    w.writerows(rows)
print(f"Wrote {len(rows)} rows to {out}")
