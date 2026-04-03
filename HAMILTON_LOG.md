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

## Current Phase: (a) — 1D Harmonic Oscillator

## Iteration Log

(iterations will be appended here by the ralph loop)
