#!/usr/bin/env python3
"""
Generate a synthetic CMU-style BVH file for testing the preprocessing pipeline.

Produces root joint rotations by integrating a smoothly varying angular velocity,
so the data is physically plausible and the ground truth (exp_q formula) holds exactly.
"""

import sys
import numpy as np

def main():
    out_path = sys.argv[1] if len(sys.argv) > 1 else "data/09_01.bvh"
    n_frames = 3000
    dt = 1.0 / 120.0  # 120 Hz, typical mocap

    # Generate smoothly varying angular velocity (rad/s) using low-freq sine waves
    rng = np.random.default_rng(42)
    t = np.arange(n_frames) * dt
    omega_x = 0.8 * np.sin(2 * np.pi * 0.3 * t) + 0.3 * np.sin(2 * np.pi * 1.1 * t)
    omega_y = 0.6 * np.cos(2 * np.pi * 0.2 * t) + 0.2 * np.sin(2 * np.pi * 0.7 * t)
    omega_z = 0.4 * np.sin(2 * np.pi * 0.5 * t) + 0.1 * np.cos(2 * np.pi * 1.3 * t)

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

    # Build quaternion sequence [w, x, y, z]
    quats = [np.array([1.0, 0.0, 0.0, 0.0])]
    for i in range(n_frames - 1):
        omega = np.array([omega_x[i], omega_y[i], omega_z[i]])
        dq = quat_exp(omega * dt / 2.0)
        q_next = quat_mul(quats[-1], dq)
        q_next = q_next / np.linalg.norm(q_next)
        quats.append(q_next)
    quats = np.array(quats)

    # Convert quaternions [w,x,y,z] -> Euler ZXY degrees for BVH
    from scipy.spatial.transform import Rotation
    # scipy expects [x,y,z,w]
    q_xyzw = quats[:, [1, 2, 3, 0]]
    r = Rotation.from_quat(q_xyzw)
    euler_zxy = r.as_euler('ZXY', degrees=True)  # (N, 3): Z, X, Y

    # Write BVH file (minimal CMU-style skeleton with just root)
    with open(out_path, 'w') as f:
        f.write("HIERARCHY\n")
        f.write("ROOT Hips\n")
        f.write("{\n")
        f.write("\tOFFSET 0.00 0.00 0.00\n")
        f.write("\tCHANNELS 6 Xposition Yposition Zposition Zrotation Xrotation Yrotation\n")
        f.write("\tEnd Site\n")
        f.write("\t{\n")
        f.write("\t\tOFFSET 0.00 10.00 0.00\n")
        f.write("\t}\n")
        f.write("}\n")
        f.write("MOTION\n")
        f.write(f"Frames: {n_frames}\n")
        f.write(f"Frame Time: {dt:.6f}\n")
        for i in range(n_frames):
            zr, xr, yr = euler_zxy[i]
            # Xpos Ypos Zpos Zrot Xrot Yrot
            f.write(f"0.000000 0.000000 0.000000 {zr:.6f} {xr:.6f} {yr:.6f}\n")

    print(f"Written {n_frames} frames to {out_path} (dt={dt:.6f}s)")


if __name__ == "__main__":
    main()
