# Hamiltonian Discovery — Research Log

## Problem
Discover Hamiltonian functions H(q,p) from trajectory data using multi-typed GADT
symbolic regression with symplectic residual loss.

## Ground Truths
- (a) H = 0.5·p² + 0.5·k·q²  (k=4.0, harmonic oscillator)
- (b) H = Lx²/(2I₁) + Ly²/(2I₂) + Lz²/(2I₃)  (rigid body, I=(1,2,3))
- (c) H = (|p₁|² + |p₂|²)/2 − 1/|q₁−q₂|  (two-body gravity, G=m=1)

## Success Criteria
| Phase | Condition |
|-------|-----------|
| (a)   | c₁·p² + c₂·q², c₁∈[0.48,0.52], c₂∈[1.9,2.1], loss<1e-4 |
| (b)   | c₁·lx²+c₂·ly²+c₃·lz², c₁∈[0.475,0.525], c₂∈[0.228,0.272], c₃∈[0.152,0.182], loss<1e-4 |
| (c)   | KE + PE terms, c₁∈[0.48,0.52], c₂∈[-1.05,-0.95], loss<1e-3 |

## Current Phase: (b) — 3D Rigid Body

## Iteration Log

### Phase (a) Iteration 1 — 2026-04-03

**Runs:** 10
**Changes applied:**
- Fixed k=4.0 (originally varied k∈[3.8,4.2], which creates irreducible loss floor Var(k)·E[q²]≈0.013)
- 300-point subsampling during search (3000 pts → ~10 rounds in 300s, too slow)
- Added `optimizeConsts` call after `parallelTempering` returns
- Internal wall-time 270s + external timeout 380s (gap for const opt to run)

**Trees found:** All 10 algebraically equivalent to `0.5·p² + 2·q²` (complex structural forms)
**Failure modes:** None — all 10 succeeded
**Success rate:** 10/10 = 100%
**Energy range (full dataset):** 1.6e-21 to 1.7e-12 (all << 1e-4)

**Key insight (Failure Mode FA-1 diagnosed and fixed):**
With k varying per trajectory, the symplectic residual for any fixed-coefficient tree has an irreducible floor of `Var(k)·E[q²] ≈ 0.0133`. The correct tree `c₁p²+c₂q²` cannot achieve loss < 1e-4 with varying k because k is different per trajectory but c₂ must be a single constant. Fix: use fixed k=4.0 so the true Hamiltonian H=0.5p²+2q² achieves exactly zero loss.

**Known structural issue:** Found trees express `0.5p²+2q²` via algebraic identities:
- `safeRecip(safeRecip(x)) = x` (double reciprocal)
- `safeRecip(sq(safeRecip(x))) = x²` (recip-sq-recip = square)
- `neg(neg(x)) = x`, `(a - a) = 0` (cancellation)
The expression is correct but not in canonical form. Full algebraic normalization requires e-graph rewriting (deferred).

**Phase (a): COMPLETE — advancing to phase (b)**

**Note on Approach B (symbolic differentiation):** Deferred. The `diffTree` function requires chain-rule handling for all GADT constructors with existential types. Implemented after all phases complete to compare speed vs. finite differences.
