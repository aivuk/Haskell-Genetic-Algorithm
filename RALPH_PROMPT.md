# Autonomous Research Loop: Typed Symbolic Regression Improvement

## Project
Working directory: /home/aivuk/Haskell-Genetic-Algorithm
Goal: iteratively improve a multi-typed symbolic regression system until it
reliably discovers the quaternion integration formula `exp_q(0.5 * dt * omega)`.
The end product is both improved code AND a written article.

## Permissions granted — do all of the following without asking:
- Edit any file in /home/aivuk/Haskell-Genetic-Algorithm/
- Run `cabal build`, `cabal run`, `python3`, shell commands
- Commit to git (small focused commits after each change)
- Write/append to RESEARCH_LOG.md, benchmarks/results.csv, article/DRAFT.md
- Regenerate data/mocap_sample.csv via the generator script

## Key files
- `main-mocap.hs` — the SR engine (edit this to improve the method)
- `scripts/generate_synthetic_bvh.py` — data generator (edit if data needs changing)
- `RESEARCH_LOG.md` — running research log (append each iteration)
- `benchmarks/results.csv` — quantitative results (append each run)
- `article/DRAFT.md` — the article being written (update throughout)

---

## Each Iteration — follow this procedure exactly

### Step 1: Regenerate data
```bash
/home/aivuk/.venv-mocap/bin/python3 scripts/generate_synthetic_bvh.py data/mocap_sample.csv
```

### Step 2: Run a timed search (5 minutes max per run, 3 runs per iteration)
```bash
timeout 300 cabal run genetic-algorithm-mocap -- +RTS -N4
```
Capture: energy printed, best tree string, whether it stopped by energy target or timeout.
Do this 3 times, record all 3 results.

### Step 3: Analyse each tree mathematically
For each tree found:
a. Simplify algebraically step by step (show your work)
b. Classify the failure mode using RESEARCH_LOG.md failure mode table (F1–F5)
   - If it matches no known mode, define a new one (F6, F7, ...)
c. Check: does the simplified form equal `exp_q(0.5·dt·omega)`?
   - YES → success for this run
   - NO → identify which assumption broke

### Step 4: Determine the fix
Based on the dominant failure mode across 3 runs, select ONE targeted fix.
Priority order (apply the first that applies):
1. If F1 (identity) → increase angular velocity amplitude in data generator
2. If F2 (norm trick) → norm_v should already be removed; check if it crept back;
   also check if ‖ω‖ variance is sufficient across 300 search points
3. If F3 (no dt) → verify dt varies across frames; check V.take 300 is not biased
4. If F4 (dead branch) → foldConstants is implemented; verify it is called; consider
   adding more rewrite rules for x add_v neg_v(x) = zero
5. If F5 (wrong scale) → increase optimizeConsts passes from 3 to 5; widen search radius
6. If consistently correct but complex → reduce scDepth to 3 to bias toward simpler trees
7. If energy plateaus early → increase scTemps chain count or temperature range
8. If search too slow → reduce V.take size further (try 150 pts)
9. If all 3 runs succeed → run 10 runs, record success rate, move to next improvement

Only make ONE change per iteration. Record what you changed and why.

### Step 5: Implement the fix
Edit the relevant file. Build to verify:
```bash
cabal build genetic-algorithm-mocap 2>&1 | tail -5
```
If build fails, fix it before proceeding.

### Step 6: Record results
Append to benchmarks/results.csv:
```
<iteration>,<run 1-3>,<energy_search>,<energy_full>,<uses_omega T/F>,<uses_dt T/F>,<failure_mode>,<change_made>,<simplified_tree>
```

Append to RESEARCH_LOG.md:
```markdown
### Iteration N — YYYY-MM-DD

**Runs:** 3
**Trees found:** [simplified form of each]
**Failure modes:** [F1/F2/... for each run]
**Dominant failure:** [most common]
**Fix applied:** [description of change]
**Before/after energy:** [numbers]
**Insight:** [one sentence on what this reveals about the search]
```

### Step 7: Update article draft
Append to or update article/DRAFT.md. The article has these sections — fill them
incrementally as evidence accumulates:

```
1. Introduction — why typed SR, what gap it fills
2. Related Work — DataHaskell SR, standard GP, parallel tempering
3. Method — PTree GADT, typed function pool, parallel tempering with SearchConfig
4. The Mocap Experiment — problem setup, ground truth, data generation
5. Results — table of iterations, failure modes found, fixes applied
6. Discussion — what the typed approach prevented vs enabled vs missed
7. Conclusion — summary and future work (richer type systems: matrices, tensors)
```

Do not fabricate results. Only write what has been observed in actual runs.

### Step 8: Commit
```bash
git add -A
git commit -m "research: iteration N — <one-line summary of finding>"
```

---

## Stopping Conditions — emit the promise when ALL are true:

1. **Convergence**: at least 8 out of 10 consecutive runs find a tree that simplifies
   to `exp_q(c·dt·omega)` with c ∈ [0.49, 0.51] and full-dataset energy < 1e-5
2. **Article**: all 7 sections of article/DRAFT.md are written with real results,
   at least 500 words total, results table has ≥ 5 iterations
3. **Benchmarks**: benchmarks/results.csv has ≥ 15 rows, showing clear improvement
   trend from iteration 1 to latest
4. **Code quality**: main-mocap.hs compiles cleanly and runs end-to-end

When all four conditions are met, output:
```
<promise>RESEARCH COMPLETE</promise>
```

---

## Recovery — if something goes wrong

- Build failure → read the error, fix it, rebuild before proceeding
- Runtime crash → read the error, check if it's a data issue (regenerate) or code bug (fix)
- All 3 runs hit timeout with no improvement → this is stagnation; apply fix #7 (more chains)
  or fix #8 (fewer points), then try again
- A fix makes things worse → revert with `git checkout main-mocap.hs`, log it as a negative
  result, try a different fix
- If stuck on same failure mode for 3+ iterations → log it as a hard problem, try a
  fundamentally different approach (e.g., change search algorithm parameters)

---

## Important constraints

- Each iteration = one fix. Do not batch multiple changes.
- Do not modify the ground truth (the formula exp_q(0.5*dt*omega) is correct).
- Do not "help" the search by hard-coding the answer structure.
- The success criteria must be met by the search algorithm itself, not by special-casing.
- Be honest in the article — include failed attempts, not just successes.
- Max --max-iterations for ralph-loop is 30. Track iteration number in RESEARCH_LOG.md.
