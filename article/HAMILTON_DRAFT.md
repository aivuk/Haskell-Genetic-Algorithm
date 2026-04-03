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

Ground truth: `H = 0.5·p² + 2·q²` (k=4.0 fixed).
Success: tree algebraically equivalent to `c₁·p² + c₂·q²`, c₁∈[0.48,0.52], c₂∈[1.9,2.1], loss<1e-4.

### 3.1 Iteration Table

| Iter | Change | Runs | Success | Rate | Dominant Failure |
|------|--------|------|---------|------|------------------|
| 0 | baseline (k varying, 3000 pts in energy) | 1 | 0/1 | 0% | FA-1: irreducible loss floor ~1.37e-2 |
| 1 | fixed k=4.0, 300-pt subsample, optimizeConsts | 10 | 10/10 | 100% | none |

### 3.2 Failure Mode FA-1: Varying-k Loss Floor

When k varies per trajectory (k ∈ [3.8, 4.2]), the symplectic residual for any fixed-coefficient
tree `c₁p² + c₂q²` has an irreducible minimum of `Var(k) × E[q²] ≈ 0.013`, far above the 1e-4
target. This is because c₂ = k/2 is trajectory-specific, but the tree encodes a single constant.
Fix: use fixed k=4.0 so H=0.5p²+2q² achieves exactly zero residual.

### 3.3 Representative Found Trees

All 10 runs found trees algebraically equivalent to `H = 0.5p² + 2q²` via identities:
`safeRecip(sq(safeRecip(x))) = x²`, `safeRecip(safeRecip(x)) = x`, `a + neg(a) = 0`.

Full-dataset energies: 1.6e-21 to 1.7e-12 (all << 1e-4). Coefficients: c₁=0.5, c₂=2.0 ✓.

## 4. Results — Phase (b): 3D Rigid Body

Ground truth: `H = Lx²/2 + Ly²/4 + Lz²/6` (I=(1,2,3)).
Revised success criterion: full-dataset symplectic residual < 1e-4 (see Section 4.2).

### 4.1 Iteration Table

| Iter | Change | Runs | Success | Rate | Notes |
|------|--------|------|---------|------|-------|
| 0 | depth=3, all pool ops | 1 | 0/1 | 0% | FB-1: depth=3 insufficient for 3 terms |
| 0b | depth=4, safeRecip removed | 1 | 0/1 | 0% | FB-2: too slow (~70 rounds in 270s) |
| 1 | sq_lx/ly/lz leaves, no sq/safeRecip unary, depth=3 | 10 | 10/10 | 100% | — |

### 4.2 Degeneracy of the Rigid Body Hamiltonian

The rigid body symplectic loss has a fundamental degeneracy: adding any multiple of |L|² to H
leaves the equations of motion unchanged, since L × (2λL) = 2λ(L×L) = 0. The loss function
therefore has infinitely many global minima, one for each λ. The search finds Hamiltonians in
the equivalence class {H_correct + λ|L|² : λ ∈ ℝ}. Examples found:
- λ=1/6: H = 0.333lx² + 0.083ly² (lz term vanishes)
- λ=0.25: H = 0.252lx² − 0.082lz² (ly term vanishes)
- λ≈0.33: H = 0.167lz² + 0.083ly² (near-diagonal form)

All represent the same physical dynamics. The revised success criterion is simply
full-dataset symplectic residual < 1e-4.

### 4.3 Key Engineering Findings

The original pool with `safeRecip` and `sq` unaries allowed polynomial explosion
(`sq(sq_lx) = lx^4`, etc.), causing severe overfitting. Replacing unary `sq` with
pre-computed squared leaves `sq_lx, sq_ly, sq_lz` eliminates this issue: the highest
polynomial degree in any tree is 2 (since `sq` can only be applied to `lx, ly, lz`
leaves, and `(*)(sq_lx, sq_lx) = lx^4` is still possible but doesn't form a degenerate minimum).

Full-dataset energies: 2.65e-11 to 6.54e-6 (all << 1e-4).

## 5. Results — Phase (c): Two-Body Gravity

Ground truth: `H = (|p₁|² + |p₂|²)/2 − 1/|q₁−q₂|`
Success criterion: full-dataset symplectic residual < 1e-3.

### 5.1 Iteration Table

| Iter | Change | Runs | Success | Rate | Notes |
|------|--------|------|---------|------|-------|
| 0 | 12 scalar leaves, depth=4 | 1 | 0/1 | 0% | FC-1: ~4 min/round, too slow |
| 0b | 3 pre-computed leaves (p1_sq,p2_sq,r_sq), depth=4, safeRecip+safeSqrt, 150-pt stride | 1 | 0/1 | 0% | FC-4: 40 rounds, best=2.07, no convergence |
| 1 | r_inv leaf, neg-only unary, depth=3, stepsPerSwap=200 | 10 | 10/10 | 100% | ~30 rounds, ~90s avg |

### 5.2 Failure Modes Diagnosed

**FC-1: 12 scalar leaves × depth=4 too slow.**
Each evaluation of the symplectic loss requires 4 Vec3 gradient computations (24 evalTree calls per data
point). With 12 leaves and depth=4, each Metropolis step is expensive and rounds take ~4 minutes.
Fix: Replace 12 scalar leaves with 3 pre-computed scalars: `p1_sq=|p1|²`, `p2_sq=|p2|²`, `r_sq=|q1-q2|²`.

**FC-2: p2=−p1 degeneracy in data generator.**
With `p2 = -p1`, the magnitudes satisfy `|p1|=|p2|` always, so `safeSqrt(p1_sq·p2_sq) = p1_sq`.
The search finds degenerate KE expressions that are correct algebraically but for wrong reasons.
Fix: Independent random momenta p1, p2 ∈ [−0.8, 0.8]³ (checked for bound orbits via KE+PE < −0.1).

**FC-3: 50 consecutive points all lie on the same orbit segment.**
Overfitting: subsample energy << 1e-3 but full-dataset energy ~ 6.8. Fix: stride-sample every 20th
point from 3000 total, yielding 150 diverse points across all 50 trajectories (3 per trajectory).

**FC-4: safeRecip+safeSqrt+depth=4 landscape too rugged.**
The PE term −1/|r| requires the 4-level unary chain `neg(safeRecip(safeSqrt(r_sq)))`. In random
search, building this exact chain by mutation is extremely unlikely in 40 rounds. Even with 150
diverse points, the search stalls at energy 2.07.
Fix: Pre-compute `r_inv = 1/|r|` as a leaf. The target tree becomes depth-3:
`(+)((*)(PConst_0.5)((+)(p1_sq)(p2_sq)))(neg(r_inv))`. Remove safeRecip and safeSqrt from
the unary pool (they are no longer needed and would re-introduce the rugged landscape).

### 5.3 Representative Found Trees

All 10 runs found trees algebraically equivalent to `H = 0.5*(p1_sq+p2_sq) − r_inv`:
- Run 1 (canonical): `neg(r_inv) + 0.5000000012*(p1_sq + p2_sq)`
- Run 8 (canonical): `neg(r_inv) + neg(neg(0.5000000013*(p1_sq+p2_sq)))`
- Runs 2–7, 9–10: equivalent via double-neg, constant folding, and associativity identities.

Full-dataset energies: 1.11e-11 to 4.59e-11 (all << 1e-3). Coefficient c₁ ≈ 0.5000 ✓.

## 6. Discussion

### 6.1 Pre-computation as a Form of Domain Knowledge

A recurring theme across all three phases is the use of *pre-computed leaves* to encode domain
structure. In each case, the pre-computation eliminated a multi-level subexpression that random
mutation rarely assembles correctly:

| System | Pre-computed leaf | Replaced chain | Depth saved |
|--------|-------------------|----------------|-------------|
| Rigid body | sq_lx, sq_ly, sq_lz | sq(lx), etc. | 1 level |
| Two-body | r_inv | neg(safeRecip(safeSqrt(r_sq))) | 3 levels |

This is analogous to feature engineering in classical ML: the symbolic regressor cannot efficiently
discover a 3-level unary chain by random search within a 300s budget, but it can compose two depth-1
subexpressions (`neg` applied to a leaf) with ease. The pre-computation is not cheating — the functional
form of the leaf (e.g., that 1/|r| is a natural function of position) encodes prior physical knowledge,
analogous to choosing an appropriate kernel or basis in other ML methods.

### 6.2 Degeneracy as a Feature, Not a Bug

Two distinct degeneracies appeared, each with different physical meanings:

*Rigid body |L|² degeneracy.* The symplectic loss is invariant under H → H + f(|L|²) because
`L × ∇_L(f(|L|²)) = f'(|L|²)·(L × L) = 0`. The search finds the correct equivalence class
{H_correct + λ|L|² : λ ∈ ℝ}, any member of which produces identical dynamics. This is a genuine
physical degeneracy: any element of the class is a valid Hamiltonian for the rigid body.

*Two-body KE structure.* The tree `0.5*(p1_sq+p2_sq)` can be expressed in many algebraically
equivalent forms (double negation, distribution over sums, etc.). These are not physical degeneracies
but rather structural redundancies in the tree grammar. They are resolved by `foldConstants` and
`simplifyTree` but not fully eliminated (canonical simplification requires e-graph rewriting, deferred).

### 6.3 Data Quality and the Role of Diversity

The FC-3 failure illustrates that data diversity matters more than data quantity for symbolic regression.
50 points from a single orbit segment provide no information about how H varies across different orbital
shapes and energies; 150 stride-sampled points covering 50 trajectories provide rich cross-trajectory
constraints. This mirrors the importance of *experiment design* in classical system identification.

### 6.4 Symplectic Loss vs. Energy Conservation Loss

An alternative loss is the energy conservation loss: `Var(H(q(t), p(t)))` over a trajectory.
The symplectic loss used here is stronger: it constrains the *gradient* of H to match observed
derivatives, not just the level sets. This means a single data point (q, p, dq/dt, dp/dt) provides
gradient information in all phase-space directions simultaneously, whereas energy conservation requires
integrating over time to observe the invariant.

## 7. Conclusion

We have demonstrated that multi-typed GADT symbolic regression with a symplectic residual loss
can recover Hamiltonians for three physical systems of increasing complexity:

1. **1D Harmonic Oscillator** (H = 0.5p² + 2q²): 10/10 runs, energies 1.6e-21 to 1.7e-12.
   Key: fixed k=4.0, 300-pt subsample, optimizeConsts post-search.

2. **3D Rigid Body** (H ∈ {H_correct + λ|L|²}): 10/10 runs, energies 2.65e-11 to 6.54e-6.
   Key: pre-computed squared-momentum leaves, |L|² degeneracy identified and characterized.

3. **Two-Body Gravity** (H = 0.5(|p₁|²+|p₂|²) − 1/|r|): 10/10 runs, energies 1.11e-11 to 4.59e-11.
   Key: pre-computed r_inv leaf collapses a 4-level unary chain to a single leaf, reducing required
   depth from 4 to 3 and enabling convergence within ~90s per run.

The GADT type system provides strong correctness guarantees: trees are type-checked at construction
time, making it impossible to generate expressions with dimension or type mismatches. The existential
typing of pool entries allows heterogeneous operators (Vec3→Double, Double→Double, etc.) to coexist
in a single pool without sacrificing type safety.

The central engineering insight is that *the boundary between pre-computation and search space*
is the primary design parameter. Moving domain-specific subexpressions into pre-computed leaves
reduces effective search depth and eliminates rugged landscape features, while still leaving the
algebraic structure of the Hamiltonian to be discovered. Future work should formalize this as an
iterative *pool refinement* procedure: start with primitive leaves, identify 3+ level chains that
consistently appear in found trees, collapse them into new leaves, and re-run.
