#!/usr/bin/env python3
"""Generate two-body gravitational problem data for Hamiltonian SR."""
import numpy as np, csv, os

def accel(q1, q2):
    """Gravitational acceleration on body 1 from body 2 (G=m=1)."""
    r = q2 - q1
    dist = np.linalg.norm(r)
    return r / (dist**3)

def verlet_step(q1, q2, p1, p2, dt):
    a1 = accel(q1, q2)
    a2 = accel(q2, q1)
    p1_new = p1 + dt * a1
    p2_new = p2 + dt * a2
    q1_new = q1 + dt * p1_new
    q2_new = q2 + dt * p2_new
    return q1_new, q2_new, p1_new, p2_new

np.random.seed(42)
rows = []
for _ in range(50):
    # Generate bound initial conditions (negative total energy)
    while True:
        q1 = np.random.uniform(-2, 2, 3)
        q2 = np.random.uniform(-2, 2, 3)
        p1 = np.random.uniform(-1, 1, 3)
        p2 = -p1  # zero total momentum
        r = np.linalg.norm(q2 - q1)
        KE = 0.5 * (np.dot(p1, p1) + np.dot(p2, p2))
        PE = -1.0 / r
        if KE + PE < 0:
            break
    for step in range(60):
        # Derivatives (Hamilton's equations)
        r_vec = q2 - q1
        r_dist = np.linalg.norm(r_vec)
        dq1_dt = p1
        dq2_dt = p2
        dp1_dt = r_vec / (r_dist**3)
        dp2_dt = -r_vec / (r_dist**3)
        rows.append([
            q1[0], q1[1], q1[2],
            q2[0], q2[1], q2[2],
            p1[0], p1[1], p1[2],
            p2[0], p2[1], p2[2],
            dq1_dt[0], dq1_dt[1], dq1_dt[2],
            dq2_dt[0], dq2_dt[1], dq2_dt[2],
            dp1_dt[0], dp1_dt[1], dp1_dt[2],
            dp2_dt[0], dp2_dt[1], dp2_dt[2],
        ])
        q1, q2, p1, p2 = verlet_step(q1, q2, p1, p2, 0.01)

out = os.path.join(os.path.dirname(__file__), '..', 'data', 'twobody.csv')
os.makedirs(os.path.dirname(out), exist_ok=True)
cols = ['q1x','q1y','q1z','q2x','q2y','q2z',
        'p1x','p1y','p1z','p2x','p2y','p2z',
        'dq1x_dt','dq1y_dt','dq1z_dt','dq2x_dt','dq2y_dt','dq2z_dt',
        'dp1x_dt','dp1y_dt','dp1z_dt','dp2x_dt','dp2y_dt','dp2z_dt']
with open(out, 'w', newline='') as f:
    w = csv.writer(f)
    w.writerow(cols)
    w.writerows(rows)
print(f"Wrote {len(rows)} rows to {out}")
