#!/usr/bin/env python3
"""
Preprocess a CMU BVH motion capture file into a CSV suitable for
quaternion symbolic regression.

Output columns: omega_x, omega_y, omega_z, dt, dq_w, dq_x, dq_y, dq_z

Usage:
    python preprocess_mocap.py input.bvh output.csv
"""

import sys
import numpy as np
from scipy.spatial.transform import Rotation


def parse_bvh(path):
    """Return (frame_time, euler_angles_deg) for the root joint.

    Assumes CMU-style BVH where root channels are:
        Xposition Yposition Zposition Zrotation Xrotation Yrotation
    Returns euler angles in (Z, X, Y) order, degrees.
    """
    with open(path) as f:
        lines = f.readlines()

    # Find frame time and motion data start
    frame_time = None
    motion_start = None
    for i, line in enumerate(lines):
        s = line.strip()
        if s.startswith("Frame Time:"):
            frame_time = float(s.split(":")[1].strip())
        if s == "MOTION":
            motion_start = i
    assert frame_time is not None, "Could not find 'Frame Time' in BVH"
    assert motion_start is not None, "Could not find 'MOTION' in BVH"

    # Root joint: Xpos Ypos Zpos Zrot Xrot Yrot -> rot at indices 3,4,5
    rot_cols = (3, 4, 5)   # Zrot, Xrot, Yrot

    # Parse frame data (skip "MOTION", "Frames: N", "Frame Time: ...")
    data_lines = lines[motion_start + 3:]
    frames = []
    for line in data_lines:
        line = line.strip()
        if not line:
            continue
        vals = list(map(float, line.split()))
        zr, xr, yr = vals[rot_cols[0]], vals[rot_cols[1]], vals[rot_cols[2]]
        frames.append([zr, xr, yr])

    return frame_time, np.array(frames)   # (N, 3) in ZXY degrees


def euler_to_quats(euler_zxy_deg):
    """Convert (N, 3) ZXY Euler angles in degrees to (N, 4) quaternions [w, x, y, z]."""
    # scipy uses [x,y,z,w]; we reorder to [w,x,y,z] for linear/Haskell convention
    r = Rotation.from_euler('ZXY', euler_zxy_deg, degrees=True)
    q_xyzw = r.as_quat()                 # (N, 4) as [x, y, z, w]
    q_wxyz = q_xyzw[:, [3, 0, 1, 2]]    # reorder to [w, x, y, z]
    return q_wxyz


def ensure_continuity(quats):
    """Flip sign of q[i] if it is on the opposite hemisphere from q[i-1]."""
    out = quats.copy()
    for i in range(1, len(out)):
        if np.dot(out[i], out[i - 1]) < 0:
            out[i] = -out[i]
    return out


def quat_conjugate(q):
    """Conjugate of [w, x, y, z] quaternion."""
    return np.array([q[0], -q[1], -q[2], -q[3]])


def quat_mul(q1, q2):
    """Hamilton product of two [w, x, y, z] quaternions."""
    w1, x1, y1, z1 = q1
    w2, x2, y2, z2 = q2
    return np.array([
        w1*w2 - x1*x2 - y1*y2 - z1*z2,
        w1*x2 + x1*w2 + y1*z2 - z1*y2,
        w1*y2 - x1*z2 + y1*w2 + z1*x2,
        w1*z2 + x1*y2 - y1*x2 + z1*w2,
    ])


def quat_log(q):
    """Quaternion logarithm of a unit quaternion [w, x, y, z].
    Returns pure vector [vx, vy, vz].
    """
    w = np.clip(q[0], -1.0, 1.0)
    v = q[1:]
    v_norm = np.linalg.norm(v)
    if v_norm < 1e-10:
        return np.zeros(3)
    theta = np.arccos(w)
    return (theta / v_norm) * v


def quat_exp(v):
    """Quaternion exponential of a pure vector v -> unit quaternion [w,x,y,z]."""
    h = np.linalg.norm(v)
    if h < 1e-10:
        return np.array([1.0, 0.0, 0.0, 0.0])
    return np.array([np.cos(h), *(np.sin(h) / h * v)])


def compute_rows(quats, dt):
    """Compute (omega, dt, delta_q) rows from a quaternion sequence."""
    rows = []
    for i in range(1, len(quats)):
        q_prev = quats[i - 1]
        q_curr = quats[i]
        dq = quat_mul(quat_conjugate(q_prev), q_curr)
        # Normalise dq to correct float drift
        dq = dq / np.linalg.norm(dq)
        # omega = 2 * log(dq) / dt
        log_dq = quat_log(dq)
        omega = 2.0 * log_dq / dt
        rows.append((omega[0], omega[1], omega[2], dt,
                     dq[0], dq[1], dq[2], dq[3]))
    return rows


def validate(rows):
    """Reconstruct dq from omega via exp_q(omega*dt/2) and check round-trip error."""
    errors = []
    for (ox, oy, oz, dt, dw, dx, dy, dz) in rows:
        omega = np.array([ox, oy, oz])
        dq_target = np.array([dw, dx, dy, dz])
        dq_hat = quat_exp(omega * dt / 2.0)
        # Handle double cover
        d = abs(np.dot(dq_hat, dq_target))
        errors.append(1.0 - d * d)
    mean_err = float(np.mean(errors))
    print(f"Validation: mean quaternion loss = {mean_err:.2e}  (target < 1e-6)")
    assert mean_err < 1e-6, f"Round-trip validation FAILED: mean loss = {mean_err}"


def main():
    if len(sys.argv) != 3:
        print("Usage: python preprocess_mocap.py input.bvh output.csv")
        sys.exit(1)

    bvh_path, csv_path = sys.argv[1], sys.argv[2]

    print(f"Parsing {bvh_path} ...")
    dt, euler = parse_bvh(bvh_path)
    print(f"  {len(euler)} frames, frame_time={dt:.6f}s")

    quats = euler_to_quats(euler)
    quats = ensure_continuity(quats)

    rows = compute_rows(quats, dt)
    print(f"  {len(rows)} transition rows")

    validate(rows)

    with open(csv_path, "w") as f:
        f.write("omega_x,omega_y,omega_z,dt,dq_w,dq_x,dq_y,dq_z\n")
        for row in rows:
            f.write(",".join(f"{v:.10f}" for v in row) + "\n")

    print(f"Written to {csv_path}")


if __name__ == "__main__":
    main()
