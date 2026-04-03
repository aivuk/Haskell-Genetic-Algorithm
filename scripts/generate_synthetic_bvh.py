#!/usr/bin/env python3
"""
Generate a synthetic CMU-style BVH file for testing the preprocessing pipeline.

Produces root joint rotations by integrating a smoothly varying angular velocity,
so the data is physically plausible and the ground truth (exp_q formula) holds exactly.
"""

import sys
import numpy as np

def main():
    out_path = sys.argv[1] if len(sys.argv) > 1 else "data/mocap_sample.csv"
    n_frames = 3000
    # Vary dt between frames (50-200 Hz) so the tree cannot absorb dt into constants
    rng_dt = np.random.default_rng(7)
    dts = rng_dt.uniform(1.0/200.0, 1.0/50.0, size=n_frames)

    # Generate smoothly varying angular velocity (rad/s) using low-freq sine waves
    rng = np.random.default_rng(42)
    t = np.cumsum(dts)  # time axis built from variable dts
    # Amplitude modulated by a slowly varying envelope so ||omega|| ranges
    # from ~0.1 to ~15 rad/s — prevents SR from exploiting constant ||omega||
    envelope = 1.0 + 7.0 * (0.5 + 0.5 * np.sin(2 * np.pi * 0.05 * t))
    omega_x = envelope * (0.6 * np.sin(2 * np.pi * 0.3 * t) + 0.3 * np.sin(2 * np.pi * 1.1 * t))
    omega_y = envelope * (0.5 * np.cos(2 * np.pi * 0.2 * t) + 0.2 * np.sin(2 * np.pi * 0.7 * t))
    omega_z = envelope * (0.3 * np.sin(2 * np.pi * 0.5 * t) + 0.1 * np.cos(2 * np.pi * 1.3 * t))

    # Integrate to get quaternion sequence using exp_q
    def quat_exp(v):
        h = np.linalg.norm(v)
        if h < 1e-10:
            return np.array([1.0, 0.0, 0.0, 0.0])
        return np.array([np.cos(h), *(np.sin(h) / h * v)])

    def quat_mul(q1, q2):
        w1, x1, y1, z1 = q1
        w2, x2, y2, z2 = q2
        return np.array([
            w1*w2 - x1*x2 - y1*y2 - z1*z2,
            w1*x2 + x1*w2 + y1*z2 - z1*y2,
            w1*y2 - x1*z2 + y1*w2 + z1*x2,
            w1*z2 + x1*y2 - y1*x2 + z1*w2,
        ])

    # Build quaternion sequence [w, x, y, z] using per-frame dt
    quats = [np.array([1.0, 0.0, 0.0, 0.0])]
    for i in range(n_frames - 1):
        omega = np.array([omega_x[i], omega_y[i], omega_z[i]])
        dq = quat_exp(omega * dts[i] / 2.0)
        q_next = quat_mul(quats[-1], dq)
        q_next = q_next / np.linalg.norm(q_next)
        quats.append(q_next)
    quats = np.array(quats)

    # Write CSV directly (bypass BVH — variable dt can't be stored in BVH format)
    # Columns: omega_x, omega_y, omega_z, dt, dq_w, dq_x, dq_y, dq_z
    rows = []
    for i in range(n_frames - 1):
        omega = np.array([omega_x[i], omega_y[i], omega_z[i]])
        q_prev, q_curr = quats[i], quats[i + 1]

        def quat_conjugate(q):
            return np.array([q[0], -q[1], -q[2], -q[3]])

        dq = quat_mul(quat_conjugate(q_prev), q_curr)
        dq = dq / np.linalg.norm(dq)
        rows.append((omega[0], omega[1], omega[2], dts[i],
                     dq[0], dq[1], dq[2], dq[3]))

    # Validate round-trip
    def quat_exp_v(v):
        h = np.linalg.norm(v)
        if h < 1e-10:
            return np.array([1.0, 0.0, 0.0, 0.0])
        return np.array([np.cos(h), *(np.sin(h) / h * v)])

    errors = []
    for (ox, oy, oz, dt_i, dw, dx, dy, dz) in rows:
        dq_hat = quat_exp_v(np.array([ox, oy, oz]) * dt_i / 2.0)
        dq_tgt = np.array([dw, dx, dy, dz])
        d = abs(np.dot(dq_hat, dq_tgt))
        errors.append(1.0 - d * d)
    mean_err = float(np.mean(errors))
    print(f"Validation: mean loss = {mean_err:.2e}  (target < 1e-6)")
    assert mean_err < 1e-6, f"FAILED: {mean_err}"

    with open(out_path, 'w') as f:
        f.write("omega_x,omega_y,omega_z,dt,dq_w,dq_x,dq_y,dq_z\n")
        for row in rows:
            f.write(",".join(f"{v:.10f}" for v in row) + "\n")

    dt_mean = float(np.mean(dts))
    print(f"Written {len(rows)} rows to {out_path} (dt varies {dts.min():.4f}–{dts.max():.4f}s, mean {dt_mean:.4f}s)")


if __name__ == "__main__":
    main()
