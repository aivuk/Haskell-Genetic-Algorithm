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
