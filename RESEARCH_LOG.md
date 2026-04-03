# Typed Symbolic Regression — Research Log

## Problem
Discover the quaternion integration formula `exp_q(0.5 * dt * omega)` from
motion capture data using a multi-typed GADT expression tree with parallel
tempering search.

## Ground Truth
`Δq = exp_q(0.5 · dt · ω)`

where:
- `ω :: Vec3` — angular velocity (rad/s)
- `dt :: Double` — variable timestep (0.005–0.020s)
- `exp_q(v) = cos(‖v‖) + sin(‖v‖)/‖v‖ · v`

## Success Criteria (per run)
- Tree uses both `omega` AND `dt` leaves
- Simplified tree is semantically equivalent to `exp_q(c · dt · omega)` with `c ∈ [0.49, 0.51]`
- Energy on full dataset (2999 pts) after constant opt < 1e-5

## Known Failure Modes
| ID | Name | Pattern | Diagnosis |
|----|------|---------|-----------|
| F1 | Identity | `exp_q(x add_v neg_v(x))` | trivial zero-arg; small-rotation data |
| F2 | NormTrick | `exp_q(c·dt·‖ω‖·ω)` | exploits constant ‖ω‖; remove norm_v or vary ‖ω‖ |
| F3 | ConstDt | tree has no `dt` leaf | exploits fixed timestep; vary dt |
| F4 | DeadBranch | subtree evaluates to 0/identity for all inputs | constant folding catches; also a sign of overfitting |
| F5 | WrongScale | correct structure but c far from 0.5 | constant optimizer needs more passes/range |

## Known Additional Failure Modes
| ID | Name | Pattern | Diagnosis |
|----|------|---------|-----------|
| F6 | ApproxExtraDt | `exp_q(c*dt*(1-dt)*omega)` | extra dt term makes formula approximately correct for small dt; tree too deep |

## Iteration Log

### Iteration 1 — 2026-04-02

**Runs:** 3
**Trees found:**
  - Run 1: `exp_q(0.4999·dt·omega)` (via double negation of constants)
  - Run 2: `exp_q(0.4999·dt·omega)` (via double-negation of omega)
  - Run 3: `exp_q(0.5077·dt·(1-dt)·omega)` — extra dt term

**Failure modes:** SUCCESS, SUCCESS, F6(ApproxExtraDt)
**Dominant failure:** F6 (complex tree with redundant dt factor)
**Fix applied:** Reduced scDepth from 5 to 3 to bias toward simpler trees
**Before energy (search/full):** 8.65e-6/~0, 1.4e-5/~0, 2.1e-5/1e-8
**Insight:** 2/3 runs immediately found the exact formula; depth-5 trees occasionally discover approximate solutions using dt twice in distinct positions.

### Iteration 2 — 2026-04-02

**Runs:** 3
**Trees found:**
  - Run 1: `exp_q(0.4781·dt·(1+3·dt)·omega)` (uses add_v to combine omega + 3·dt·omega)
  - Run 2: `exp_q(0.5·dt·omega)` (via neg_v(2ω) + 3ω = ω, double-negation of dt)
  - Run 3: `exp_q(0.5·dt·omega)` (clean: dt scale_v (0.5 scale_v omega))

**Failure modes:** F6(ApproxExtraDt), SUCCESS, SUCCESS
**Dominant failure:** F6 — all F6 cases use `add_v` to combine omega with dt-scaled omega
**Fix applied:** Removed `add_v` from mocapBins function pool; the correct formula never needs vector addition
**Before energy:** 5e-6/5e-8, 4.7e-6/~0, 4.7e-5/~0
**Insight:** Depth=3 reduced F6 frequency slightly; the root cause is `add_v` allowing omega to appear multiple times with different dt coefficients.

### Iteration 3 — 2026-04-02

**Runs:** 3 initial + 10 extended (all 3 succeeded → ran 10)
**Trees found:**
  - All 3 initial: `exp_q(0.5·dt·omega)` (various sign/negation wrappers)
  - Extended runs 2-10: `exp_q(0.5·dt·omega)` (9/10)
  - Extended run 1: complex `q_mul` chain — F7

**Failure modes:** 9× SUCCESS, 1× F7(QMulComplex)
**Dominant failure:** F7 — `q_mul` allows building complex quaternion products that achieve energy ~0 but don't simplify to the target form
**Fix applied:** Removed `q_mul` from mocapBins; correct formula `exp_q(0.5·dt·omega)` is a single Quat expression and never needs quaternion multiplication
**10-run success rate:** 9/10 = 90%
**Insight:** Removing `add_v` nearly eliminated failures; only 1/10 runs used `q_mul` to build a complex equivalent. The correct formula uses none of the binary Quat/Vec3 operations — only scalar multiplication and exp_q.

### Iteration 4 — 2026-04-02

**Runs:** 10 (ran directly since iteration 3 reached 9/10)
**Trees found:** All 10 found `exp_q(0.5·dt·omega)` (with various sign/negation wrappers)
**Failure modes:** 10× SUCCESS
**Dominant failure:** None
**Fix applied:** Removed `q_mul` from mocapBins; target formula is a single exp_q call with no quaternion arithmetic
**10-run success rate:** 10/10 = 100%
**Insight:** With the minimal function pool {(*), scale_v, exp_q, neg_v, neg, omega, dt}, the system achieves 100% success. The key insight: the target formula's type structure (Double×Vec3→Quat via exp_q) directly constrains the search to the correct form.

### Iteration 5 — 2026-04-02

**Runs:** 10
**Trees found:** All 10 found `exp_q(0.5·dt·omega)` (some with neg/neg_v wrappers)
**Failure modes:** 10× SUCCESS
**Fix applied:** Added `simplifyTree` post-processing pass that folds `neg(PConst c)` → `PConst (-c)`, absorbing sign information directly into constants for cleaner output
**10-run success rate:** 10/10 = 100%
**Insight:** The simplifyTree pass reduces visual noise in tree output. Remaining double-neg_v patterns require structural GADT rewrites (complex with existential types); they are cosmetically verbose but mathematically identical to the canonical form.
