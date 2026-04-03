# Hamiltonian Discovery via Typed Symbolic Regression

## 1. Introduction

Discovering conserved quantities from trajectory data is a fundamental problem in
physics-informed machine learning. The Hamiltonian H(q,p) of a mechanical system
encodes all dynamics via Hamilton's equations:

```
dq/dt = ∂H/∂p,    dp/dt = −∂H/∂q
```

Given observed trajectories (q(t), p(t), dq/dt, dp/dt), we seek to recover H
without knowing its functional form a priori. This is the Hamiltonian identification
problem.

We extend the multi-typed GADT symbolic regression system described in the companion
paper (typed quaternion integration discovery) to Hamiltonian identification, using
a symplectic residual loss that measures how well a candidate H satisfies Hamilton's
equations via finite-difference gradients.

Three physical systems of increasing complexity are tackled in series:
- **(a) 1D Harmonic Oscillator** — `H = 0.5·p² + 0.5·k·q²`
- **(b) 3D Rigid Body** — `H = Lx²/(2I₁) + Ly²/(2I₂) + Lz²/(2I₃)`
- **(c) Two-Body Gravity** — `H = (|p₁|² + |p₂|²)/2 − 1/|q₁−q₂|`

## 2. Method

### 2.1 Symplectic Residual Loss

For a candidate tree H :: PTree inp Double, the finite-difference gradient w.r.t.
a scalar coordinate x is:

```
∂H/∂x ≈ (H(x+ε) − H(x−ε)) / (2ε)    ε = 1e-5
```

The residual loss per point is:

```
L = (q̇ − ∂H/∂p)² + (ṗ + ∂H/∂q)²
```

For vector-valued coordinates (Vec3), each component is perturbed separately
(6 evalTree calls per Vec3 field).

### 2.2 Function Pool Design

Each system uses a curated pool of operators matched to the expected structure of H:

| System | Binary | Unary | Leaves |
|--------|--------|-------|--------|
| Harmonic | (+),(*),(-) | sq, neg, safeRecip | q, p |
| Rigid Body | (+),(*) | sq, neg, safeRecip | lx, ly, lz |
| Two-Body | (+),(*) | sq, neg, safeRecip, safeSqrt | q1x..z, q2x..z, p1x..z, p2x..z |

### 2.3 Search Configuration

Parallel tempering with 12 chains, log-spaced temperatures [0.001, 10.0], 200
Metropolis steps per swap, wall-time limit 300s. Depth 3 for systems (a) and (b),
depth 4 for system (c).

## 3. Results — Phase (a): 1D Harmonic Oscillator

(Results will be filled in by the ralph loop)

| Iter | Change | Runs | Success | Success Rate | Dominant Failure |
|------|--------|------|---------|--------------|------------------|

## 4. Results — Phase (b): 3D Rigid Body

(Results will be filled in by the ralph loop)

| Iter | Change | Runs | Success | Success Rate | Dominant Failure |
|------|--------|------|---------|--------------|------------------|

## 5. Results — Phase (c): Two-Body Gravity

(Results will be filled in by the ralph loop)

| Iter | Change | Runs | Success | Success Rate | Dominant Failure |
|------|--------|------|---------|--------------|------------------|

## 6. Discussion

(Discussion will be filled in after results are complete)

## 7. Conclusion

(Conclusion will be filled in after results are complete)
