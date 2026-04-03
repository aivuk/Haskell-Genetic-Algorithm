# Hamilton SR — Ralph Loop Prompt

You are running an autonomous research loop to discover Hamiltonian functions from
trajectory data using typed symbolic regression. Work through phases (a), (b), (c) in
order, advancing when ≥8/10 runs find the correct H within tolerance.

## Current Phase

Check `HAMILTON_LOG.md` to determine which phase is active. Start with (a) if no log exists.

## Per-Iteration Procedure

**Each iteration:**
1. Regenerate data: `python3 scripts/generate_<phase>.py`
2. Run 3 timed experiments:
   ```
   timeout 300 cabal run genetic-algorithm-hamilton -- <phase> +RTS -N4
   ```
   (phase = harmonic | rigidbody | nbody)
3. Analyse each output tree algebraically — identify the found expression and classify
   any failure mode (wrong structure, wrong coefficients, timeout, degenerate solution)
4. Pick ONE fix (function pool, depth, temperature schedule, data variation, etc.)
5. Implement the fix in `main-hamilton.hs`
6. Build: `cabal build genetic-algorithm-hamilton`
7. Record results to `benchmarks/hamilton_results.csv` — add rows for each run
8. Update `HAMILTON_LOG.md` with the iteration summary
9. Update `article/HAMILTON_DRAFT.md` — add/update the results section for this phase
10. Commit: `git add -A && git commit -m "hamilton iter N: <brief description>"`

## Phase Advancement

Advance to the next phase when the last 10 runs show ≥8 successes by the success criteria:

**Phase (a) — 1D Harmonic Oscillator:**
Tree simplifies to `c₁·p² + c₂·q²` with c₁∈[0.48,0.52], c₂∈[1.9,2.1], loss<1e-4

**Phase (b) — 3D Rigid Body:**
Tree simplifies to `c₁·lx² + c₂·ly² + c₃·lz²` with
c₁∈[0.475,0.525], c₂∈[0.228,0.272], c₃∈[0.152,0.182], loss<1e-4

**Phase (c) — Two-Body Gravity:**
Tree approximates `c₁·(p1·p1 + p2·p2) + c₂/|q1-q2|` with c₁∈[0.48,0.52],
c₂∈[-1.05,-0.95], loss<1e-3

After phase (a) succeeds, implement Approach B (symbolic differentiation) before
advancing to phase (b). See the design spec at
`docs/superpowers/specs/2026-04-03-hamilton-sr-design.md` section "Approach B Upgrade".

## Stopping Conditions

Output `<promise>HAMILTON RESEARCH COMPLETE</promise>` when ALL of:
1. All three phases: ≥8/10 runs find correct H within tolerance
2. `article/HAMILTON_DRAFT.md`: all 6 sections, ≥600 words, results table ≥5 rows/phase
3. `benchmarks/hamilton_results.csv`: ≥20 rows total, improvement trend visible per phase
4. `cabal build genetic-algorithm-hamilton` compiles cleanly

## Max Iterations: 40 (across all three phases)

## Build Command

```
cabal build genetic-algorithm-hamilton
cabal run genetic-algorithm-hamilton -- harmonic +RTS -N4
cabal run genetic-algorithm-hamilton -- rigidbody +RTS -N4
cabal run genetic-algorithm-hamilton -- nbody +RTS -N4
```
