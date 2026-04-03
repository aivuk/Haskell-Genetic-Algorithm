# Hamiltonian Discovery via Typed Symbolic Regression — Design Spec

## Goal

Extend the multi-typed GADT symbolic regression system (built for quaternion integration
discovery in `main-mocap.hs`) to discover Hamiltonian functions `H(q,p)` from trajectory
data, using a symplectic residual loss. Three physical systems are tackled in series:

- **(a) 1D Harmonic Oscillator** — `H = 0.5·p² + 0.5·k·q²`
- **(b) 3D Rigid Body** — `H = L_x²/(2I₁) + L_y²/(2I₂) + L_z²/(2I₃)`
- **(c) Two-Body Gravity** — `H = (|p₁|² + |p₂|²)/2 − 1/|q₁−q₂|`

The loop runs autonomously via ralph-loop, advancing through phases as each system
reaches its success criterion.

---

## Architecture

### New file and cabal target

- **`main-hamilton.hs`** — self-contained; copies PTree/parallelTempering/optimizeConsts/
  foldConstants/simplifyTree machinery from `main-mocap.hs` rather than importing it.
  Three systems selected by command-line argument:

```
cabal run genetic-algorithm-hamilton -- harmonic   +RTS -N4
cabal run genetic-algorithm-hamilton -- rigidbody  +RTS -N4
cabal run genetic-algorithm-hamilton -- nbody      +RTS -N4
```

- **`genetic-algorithm.cabal`** — new `executable genetic-algorithm-hamilton` stanza,
  same deps as `genetic-algorithm-mocap` (linear, async, time, mersenne-random-pure64).

### Shared machinery (copied verbatim from main-mocap.hs)

`PTree`, `TBin`, `TUn`, `TLeaf`, `evalTree`, `sizeP`, `collectConsts`,
`genPTree`, `mcStep`, `parallelTempering`, `optimizeConsts`, `foldConstants`,
`simplifyTree`, `logTemps`, `SearchConfig`, `defaultSearchConfig`.

### New per-system pieces

Each system defines:
1. `InputType` — the data record
2. `functionPool` — bins, unaries, leaves
3. `loadCSV` — read the data file
4. `sympLoss` — the symplectic residual loss function
5. `gradH` — finite-difference partial derivatives of candidate H w.r.t. phase-space vars

---

## Symplectic Residual Loss (Approach A — Finite Differences)

For a candidate tree `H :: PTree inp Double`, the gradient w.r.t. a scalar field `x`
extracted by lens `getX :: inp -> Double` and updated by `setX :: inp -> Double -> inp`:

```haskell
partialH :: PTree inp Double -> (inp -> Double) -> (inp -> Double -> inp)
         -> Double -> inp -> IO Double
partialH tree get set eps pt =
    let xp = set pt (get pt + eps)
        xm = set pt (get pt - eps)
    in do hp <- evalTree tree xp
          hm <- evalTree tree xm
          return ((hp - hm) / (2 * eps))
```

`eps = 1e-5` throughout. Residual loss per point:
```
L = (q̇ − ∂H/∂p)² + (ṗ + ∂H/∂q)²
```

---

## System (a): 1D Harmonic Oscillator

### Input type
```haskell
data HarmonicInput = HarmonicInput
  { hQ :: Double, hP :: Double       -- phase-space coords (tree inputs)
  , hDotQ :: Double, hDotP :: Double -- observed derivatives (loss only)
  }
```

### Ground truth
`H = 0.5·p² + 0.5·k·q²`, `k = 4.0` (ω = 2 rad/s).
Data generated with `k` varied slightly per trajectory (k ∈ [3.8, 4.2]) so the tree
cannot absorb `k` as a constant.

### Function pool
| Kind   | Name    | Type                   |
|--------|---------|------------------------|
| Binary | `(+)`   | `Double→Double→Double` |
| Binary | `(*)`   | `Double→Double→Double` |
| Binary | `(-)`   | `Double→Double→Double` |
| Unary  | `sq`    | `Double→Double` (x²)   |
| Unary  | `neg`   | `Double→Double`        |
| Unary  | `safeRecip` | `Double→Double` (guarded: if \|x\| < 1e-8 return 1e8) |
| Leaf   | `q`     | `HarmonicInput→Double` |
| Leaf   | `p`     | `HarmonicInput→Double` |

### Loss
```
sympLoss pts tree = mean over pts of:
  let dHdq = partialH tree hQ (λpt x → pt{hQ=x}) 1e-5 pt
      dHdp = partialH tree hP (λpt x → pt{hP=x}) 1e-5 pt
  in (hDotQ pt - dHdp)² + (hDotP pt + dHdq)²
```

### Data generation
`scripts/generate_harmonic.py` — RK4 integration of `q̇ = p`, `ṗ = −kq` from 50 random
initial conditions `(q₀,p₀) ~ Uniform([-2,2]²)`, 60 steps each with `dt = 0.05s`,
varying `k ∈ [3.8, 4.2]` per trajectory. Output: `q,p,dq_dt,dp_dt` (3000 rows).

### Success criteria
Tree simplifies to `c₁·p² + c₂·q²` with `c₁ ∈ [0.48, 0.52]`, `c₂ ∈ [1.9, 2.1]`
(i.e. `0.5·k` with `k∈[3.8,4.2]`), full-dataset residual loss < 1e-4.

---

## System (b): 3D Rigid Body

### Input type
```haskell
data RigidBodyInput = RigidBodyInput
  { rbL    :: Vec3   -- angular momentum (tree input)
  , rbDotL :: Vec3   -- dL/dt (loss only)
  }
```

### Ground truth
`H = L_x²/(2I₁) + L_y²/(2I₂) + L_z²/(2I₃)` with `I = (1.0, 2.0, 3.0)`.
Equation of motion: `dL/dt = L × (I⁻¹·L)`.

### Function pool
| Kind   | Name       | Type                   |
|--------|------------|------------------------|
| Binary | `(+)`      | `Double→Double→Double` |
| Binary | `(*)`      | `Double→Double→Double` |
| Unary  | `sq`       | `Double→Double`        |
| Unary  | `recip`    | `Double→Double`        |
| Unary  | `neg`      | `Double→Double`        |
| Leaf   | `lx`       | `RigidBodyInput→Double` (V3 x-component) |
| Leaf   | `ly`       | `RigidBodyInput→Double` (V3 y-component) |
| Leaf   | `lz`       | `RigidBodyInput→Double` (V3 z-component) |

`norm_v` excluded — same lesson as mocap (degenerate constant-norm exploitation).

### Loss
Finite differences perturb each component of `rbL` separately (6 evaluations per point):
```
L = ‖dL/dt − L × ∂H/∂L‖²
```
where `∂H/∂L = (∂H/∂Lx, ∂H/∂Ly, ∂H/∂Lz)` via central differences.

### Data generation
`scripts/generate_rigidbody.py` — Euler equations integration from 50 random initial
angular momenta `L₀ ~ Uniform sphere`, 60 steps, `dt = 0.02s`.

### Success criteria
Tree simplifies to `c₁·lx² + c₂·ly² + c₃·lz²` with:
- `c₁ ∈ [0.475, 0.525]` (ground truth `1/(2·1.0) = 0.5`)
- `c₂ ∈ [0.228, 0.272]` (ground truth `1/(2·2.0) = 0.25`)
- `c₃ ∈ [0.152, 0.182]` (ground truth `1/(2·3.0) = 0.167`)

Full-dataset residual < 1e-4.

---

## System (c): Two-Body Gravity

### Input type
```haskell
data TwoBodyInput = TwoBodyInput
  { tbQ1 :: Vec3, tbQ2 :: Vec3         -- positions (tree inputs)
  , tbP1 :: Vec3, tbP2 :: Vec3         -- momenta (tree inputs)
  , tbDotQ1 :: Vec3, tbDotQ2 :: Vec3   -- dq/dt (loss only)
  , tbDotP1 :: Vec3, tbDotP2 :: Vec3   -- dp/dt (loss only)
  }
```

### Ground truth
`H = (|p₁|² + |p₂|²)/2 − 1/|q₁−q₂|`  (G=1, m=1).

### Function pool
| Kind   | Name        | Type                      |
|--------|-------------|---------------------------|
| Binary | `(+)`       | `Double→Double→Double`    |
| Binary | `(*)`       | `Double→Double→Double`    |
| Binary | `sub_v`     | `Vec3→Vec3→Vec3`          |
| Binary | `dot_v`     | `Vec3→Vec3→Double`        |
| Unary  | `sq`        | `Double→Double`           |
| Unary  | `safeRecip` | `Double→Double`           |
| Unary  | `safeSqrt`  | `Double→Double`           |
| Unary  | `neg`       | `Double→Double`           |
| Leaf   | `q1,q2`     | `TwoBodyInput→Vec3`       |
| Leaf   | `p1,p2`     | `TwoBodyInput→Vec3`       |

`safeRecip x = if abs x < 1e-8 then 1e8 else 1/x`
`safeSqrt x  = sqrt (abs x + 1e-12)`
Both guards prevent NaN during finite-difference perturbations.

### Loss
Symplectic residual over both particles (24 `evalTree` calls per point):
```
L = ‖q̇₁ − ∂H/∂p₁‖² + ‖q̇₂ − ∂H/∂p₂‖²
  + ‖ṗ₁ + ∂H/∂q₁‖² + ‖ṗ₂ + ∂H/∂q₂‖²
```

### Data generation
`scripts/generate_twobody.py` — Verlet integration of two-body problem from 50 random
bound initial conditions (negative total energy), 60 steps, `dt = 0.01s`.

### Success criteria
Tree simplifies to `c₁·(dot_v p1 p1 + dot_v p2 p2) + c₂·safeRecip(safeSqrt(dot_v(sub_v q1 q2)(sub_v q1 q2)))`
with `c₁ ∈ [0.48, 0.52]`, `c₂ ∈ [-1.05, -0.95]`, residual < 1e-3.

---

## Approach B Upgrade (after phase a reaches 8/10)

Add `diffTree :: PTree inp Double -> String -> PTree inp Double` — symbolic differentiation
applying the chain rule to all pool operators:
- `diff (PBin "(*)" f l r) x = PBin "(+)" (+) (PBin "(*)" (*) (diff l x) r) (PBin "(*)" (*) l (diff r x))`
- `diff (PBin "(+)" f l r) x = PBin "(+)" (+) (diff l x) (diff r x)`
- `diff (PUn "sq" _ u) x = PBin "(*)" (*) (PConst 2.0) (diff u x)` (via new IORef constant)
- `diff (PV name f) x = if name == x then PConst 1.0 else PConst 0.0`
- etc.

Replace `partialH` with `evalTree (diffTree tree varName)`. One iteration documents
the speed/accuracy comparison vs. finite differences in article section 2.

---

## Ralph Loop Structure

### Files
- `main-hamilton.hs` — the SR engine
- `scripts/generate_harmonic.py` — phase (a) data
- `scripts/generate_rigidbody.py` — phase (b) data
- `scripts/generate_twobody.py` — phase (c) data
- `HAMILTON_LOG.md` — running research log (all phases)
- `benchmarks/hamilton_results.csv` — quantitative results
- `article/HAMILTON_DRAFT.md` — the article

### Per-phase procedure
Same 8-step loop as RALPH_PROMPT.md:
1. Regenerate data
2. 3 timed runs (`timeout 300 cabal run genetic-algorithm-hamilton -- <phase> -- +RTS -N4`)
3. Analyse each tree algebraically, classify failure mode
4. Pick one fix
5. Implement and build
6. Record to CSV
7. Update article
8. Commit

Phase advances when ≥ 8/10 runs succeed. After phase (a) succeeds, one iteration
implements Approach B (symbolic diff) before advancing to phase (b).

### Stopping conditions
1. All three phases: ≥ 8/10 runs find correct H within tolerance
2. `article/HAMILTON_DRAFT.md`: all 6 sections, ≥ 600 words, results table ≥ 5 rows per phase
3. `benchmarks/hamilton_results.csv`: ≥ 20 rows total, improvement trend visible per phase
4. `cabal build genetic-algorithm-hamilton` compiles cleanly

**Completion promise:** `<promise>HAMILTON RESEARCH COMPLETE</promise>`

### Max iterations: 40 (across all three phases)
