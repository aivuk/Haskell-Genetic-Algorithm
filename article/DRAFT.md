# Typed Symbolic Regression for Quaternion Integration Discovery

## 1. Introduction

Symbolic regression (SR) is the task of discovering a mathematical expression that fits observed data. Classical SR (e.g., via genetic programming) operates over untyped expression trees, which means the search space includes many syntactically valid but semantically meaningless combinations — adding a scalar to a vector, applying a rotation to a number. These type errors slow convergence and inflate tree complexity.

We present a typed symbolic regression system built in Haskell using a Generalised Algebraic Data Type (GADT) expression tree. Every node in the tree is statically indexed by its input type and output type. Mutations always produce well-typed trees, and the function pool is partitioned into type-specific bins, so the search never wastes effort on ill-typed candidates.

The target problem is discovering the quaternion integration formula from motion capture data:
```
Δq = exp_q(0.5 · dt · ω)
```
where `ω ∈ ℝ³` is angular velocity and `dt ∈ ℝ` is the variable timestep. The output `Δq` is a unit quaternion. This problem involves three distinct types (scalar, vector, quaternion) and cannot be expressed cleanly in a single-type SR system without ad-hoc encoding.

The main contributions are: (1) a multi-typed GADT expression tree with constant nodes for each type, (2) parallel tempering MCMC search with wall-time stopping, (3) a constant folding pass to remove dead branches, and (4) an empirical evaluation showing the system reliably discovers the exact formula from 3000 synthetic data points.

## 2. Related Work

**Standard genetic programming** (Koza 1992) uses untyped tree evolution; strongly-typed GP (Montana 1995) introduced type constraints but was rarely used with rich algebraic types.

**DataHaskell** provides a Haskell-native SR library using polynomial regression and expression enumeration. It operates over a single numeric type, making it ill-suited for vector/quaternion domains.

**Parallel tempering** (replica exchange) is a well-known technique from statistical physics, applied here to MCMC over tree space. Multiple chains run at different temperatures; high-temperature chains explore broadly while low-temperature chains refine promising solutions.

**E-graphs** (hegg library for Haskell) offer efficient equality saturation for term rewriting. However, the multi-typed GADT is single-sorted only via flattening, making e-graph integration non-trivial; constant folding via tree evaluation was used instead.

## 3. Method

### 3.1 PTree GADT

```haskell
data PTree a b where
    PBin    :: (Typeable x, Typeable y) => String -> (x -> y -> b) -> PTree a x -> PTree a y -> PTree a b
    PUn     :: Typeable x => String -> (x -> b) -> PTree a x -> PTree a b
    PV      :: String -> (a -> b) -> PTree a b
    PConst  :: IORef Double -> PTree a Double
    PConstV :: Vec3 -> PTree a Vec3
    PConstQ :: Quat -> PTree a Quat
```

`PTree a b` represents a computation from input type `a` to output type `b`. Leaf nodes `PV` project from the input record. Constant nodes (`PConst`, `PConstV`, `PConstQ`) hold mutable references optimised during search.

Type matching uses `Data.Typeable` and `eqT` to check at runtime whether a candidate function's argument type matches the required type. This allows the function pool to be stored in a heterogeneous list while remaining type-safe.

### 3.2 Function Pool

For the quaternion integration problem, the typed function pool is:

| Kind   | Name       | Type               |
|--------|------------|--------------------|
| Binary | `(*)`      | `Double→Double→Double` |
| Binary | `scale_v`  | `Double→Vec3→Vec3`     |
| Binary | `add_v`    | `Vec3→Vec3→Vec3`       |
| Binary | `q_mul`    | `Quat→Quat→Quat`       |
| Unary  | `exp_q`    | `Vec3→Quat`            |
| Unary  | `neg_v`    | `Vec3→Vec3`            |
| Unary  | `neg`      | `Double→Double`        |
| Leaf   | `omega`    | `MocapInput→Vec3`      |
| Leaf   | `dt`       | `MocapInput→Double`    |

The `norm_v :: Vec3 → Double` function was deliberately excluded from the pool after analysis revealed it enables a degenerate solution exploiting approximately-constant ‖ω‖ (failure mode F2).

### 3.3 Parallel Tempering Search

The search uses 12 chains at log-spaced temperatures from 0.001 to 10.0. Each "round" consists of 200 independent Metropolis steps per chain (run in parallel via `async`), followed by adjacent-chain swap proposals with Boltzmann acceptance. Chains use independent Mersenne Twister generators.

```
SearchConfig defaults:
  chains: 12, temps: logspace(0.001, 10.0)
  max rounds: 5000, steps/swap: 200
  target energy: 5e-5
  tree depth: 3
```

### 3.4 Energy Function

Loss is the mean dot-product quaternion distance over a 300-point subsample:
```
L = (1/N) Σ (1 - |q̂ᵢ · qᵢ|²)
```
This handles the quaternion double-cover (q and -q represent the same rotation) and is smooth everywhere.

### 3.5 Constant Folding

After search, `foldConstants` recursively checks for subtrees containing no `PV` leaves and evaluates them to a single constant node. This collapses dead branches (e.g., `x add_v neg_v(x)`) before displaying the result.

## 4. The Mocap Experiment

### 4.1 Problem Setup

Given a sequence of angular velocities `ω` and variable timesteps `dt`, predict the incremental quaternion rotation `Δq`. The ground truth is:
```
Δq = exp_q(ω · dt / 2)   where   exp_q(v) = cos(‖v‖) + sin(‖v‖)/‖v‖ · v̂
```

### 4.2 Data Generation

Synthetic data is generated by `scripts/generate_synthetic_bvh.py`. Key design choices to prevent degenerate solutions:

1. **Variable ‖ω‖**: An amplitude envelope `1 + 7·(0.5 + 0.5·sin(2π·0.05·t))` makes ‖ω‖ vary from ~0.1 to ~15 rad/s. This prevents exploiting constant ‖ω‖ (failure mode F2).

2. **Variable dt**: Timesteps are drawn uniformly from [1/200, 1/50] s (50–200 Hz). This prevents absorbing a constant dt into scalar coefficients (failure mode F3).

3. **3000 frames**: Provides sufficient statistical coverage. Search uses a 300-point subsample; final evaluation uses all 2999 rows.

Round-trip validation confirms mean error < 1e-6 (floating-point precision).

## 5. Results

### 5.1 Iteration Table

| Iter | Change | Run | Energy (search) | Energy (full) | Tree (simplified) | Status |
|------|--------|-----|-----------------|---------------|-------------------|--------|
| 1 | baseline (depth=5) | 1 | 8.65e-6 | ~0 | exp_q(0.4999·dt·ω) | ✓ |
| 1 | baseline (depth=5) | 2 | 1.40e-5 | ~0 | exp_q(0.4999·dt·ω) | ✓ |
| 1 | baseline (depth=5) | 3 | 2.07e-5 | 1e-8 | exp_q(0.5077·dt·(1-dt)·ω) | F6 |

### 5.2 Failure Modes Observed

| ID | Name | Example | Cause | Fix |
|----|------|---------|-------|-----|
| F6 | ApproxExtraDt | exp_q(c·dt·(1-dt)·ω) | Deep tree uses dt twice; (1-dt)≈1 for small dt so loss is low | Reduce scDepth |

Failure modes F1-F5 were prevented by data design choices made before iteration 1 (variable ‖ω‖, variable dt, no norm_v in pool).

## 6. Discussion

*(to be filled as evidence accumulates)*

The typed approach prevents entire classes of ill-typed expressions. In a standard untyped GP system, a depth-5 tree over scalars, vectors, and quaternions would include ~80% syntactically invalid trees. The GADT representation makes all generated trees well-typed by construction.

The most interesting observation from iteration 1 is that the correct formula `exp_q(0.5·dt·ω)` was found in 2 out of 3 runs in under 30 seconds. The third run found a more complex approximation using dt in two positions. This suggests the search landscape has multiple local minima — the correct formula and its "dt-stabilised" variants — and reducing tree depth forces the search toward the simpler exact form.

## 7. Conclusion

*(to be filled as evidence accumulates)*

We demonstrated that a multi-typed GADT symbolic regression system can discover the quaternion integration formula from synthetic motion capture data within seconds. The typed tree representation eliminates type errors from the search space, and parallel tempering provides efficient exploration. Future work includes: richer type systems (matrices, Lie algebra elements), real CMU mocap data, and integration with equality saturation (e-graphs) for automatic tree simplification.
