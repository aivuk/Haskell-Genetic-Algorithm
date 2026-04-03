# Novel Symbolic Regression Research Plan

## Core Thesis

Current SR methods (GP, sparse regression, neural) all share the same bottleneck:
they search expression space without first characterizing what expressions are
consistent with the data's structure. We propose a **characterize-first** paradigm.

---

## The Six Novel Directions

### Direction 1: Symmetry-First SR (most immediately practical)

**Core idea**: A function's symmetries uniquely constrain its form. Detect symmetries
numerically before searching.

Symmetries to detect:
- Parity: f(-x) = ±f(x)
- Scaling: f(ax) = a^n * f(x) [homogeneity degree n]
- Periodicity: f(x+c) = f(x) [Fourier constraint]
- Lie symmetries: continuous transformations (x,y) -> (φ(x,y), ψ(x,y)) leaving graph invariant

Each detected symmetry eliminates entire expression classes from search.

**Experiment 1a**: Implement numerical symmetry detection on 1D data
**Experiment 1b**: Use detected symmetries to constrain PTree grammar
**Experiment 1c**: Benchmark: how much does the search space shrink?

Key literature gap: Lie symmetry analysis is standard for ODE analysis but
almost never used to *constrain* symbolic regression search.

---

### Direction 2: Differential Operator Inversion

**Core idea**: Instead of finding f(x), find the simplest operator L such that Lf = 0.
Then reconstruct f by integrating.

Algorithm:
1. Numerically differentiate the data 1..n times
2. Find the simplest linear combination: c0*f + c1*f' + ... + cn*f^(n) = 0
3. This is an ODE with *constant coefficients* — easy to solve symbolically
4. The solution space of that ODE IS the expression class

For non-linear ODEs: detect when d/dx[f] = g(f) for some simple g.
For example: f' = f gives exponential. f' = sqrt(1-f^2) gives arcsin/sin.

This converts SR into **ODE discovery** — much better-posed because:
- ODEs have sparse, linear structure (mostly)
- Solution spaces are well-understood
- The ODE is often far simpler than the explicit formula

**Experiment 2a**: Implement numerical differentiation cascade
**Experiment 2b**: Fit linear ODE (c0*f + c1*f' + ... = 0) via least squares
**Experiment 2c**: Test on known functions: sin, exp, Airy, Bessel, etc.
**Experiment 2d**: Extend to non-linear ODEs: f' = P(f) for polynomial P

Key open question: what's the complexity of detecting a k-th order ODE vs
searching for the explicit k-depth expression tree? Hypothesis: ODE discovery
is polynomial while expression search is exponential.

---

### Direction 3: Symbolic Residual Boosting

**Core idea**: Decompose f = f1 + f2 + ... + fn where each fi is a *simple*
symbolic expression fit to the residual.

Algorithm:
1. Find simplest f1 (size ≤ S nodes) that significantly reduces MSE
2. Compute residual r1 = f - f1
3. Find simplest f2 fitting r1
4. Iterate until residual < noise threshold

Variants:
- Additive: f = f1 + f2 + ...
- Multiplicative: f = f1 * (1 + f2 * (1 + ...))
- Mixed: detect which decomposition applies

Why this works: each stage is a much simpler SR problem. Finding a size-3
expression is trivial; finding a size-15 expression is not.

Post-processing: algebraically combine/simplify the sum of terms.

**Experiment 3a**: Implement single-step simple SR (exhaustive for size ≤ 5)
**Experiment 3b**: Implement boosting loop with residual computation
**Experiment 3c**: Test: does boosting find solutions GP misses?
**Experiment 3d**: Implement algebraic simplification of sums

Key insight: this is AdaBoost / gradient boosting but over symbolic space.
The "weak learner" is a short symbolic expression. Unlike neural boosting,
the result stays interpretable.

---

### Direction 4: Expression DAGs (Directed Acyclic Graphs)

**Core idea**: Trees force every subexpression to be used exactly once.
DAGs allow shared subexpressions, which are naturally discovered intermediate
variables.

f(x) = sin(x)^2 + sin(x)*cos(x)  →  tree needs 2 copies of sin(x)
                                   →  DAG shares 1 copy

Impact: for functions with repeated structure, the search space collapses.
The shared nodes become *automatically discovered* reusable abstractions.

Representation: adjacency list + type annotations. Each node has:
- operation (or leaf)
- output type
- children (can be shared)

Mutation operators:
- Replace a subgraph with a fresh random subgraph (same as tree)
- *New*: merge two identical/similar subgraphs into a shared node
- *New*: split a shared node into two independent copies

**Experiment 4a**: Implement DAG representation for PTree-like structure
**Experiment 4b**: Implement DAG mutation operators
**Experiment 4c**: Test on functions with repeated structure

---

### Direction 5: Multi-Scale / Renormalization Approach

**Core idea**: Inspired by renormalization group in physics. A function's behavior
at different scales reveals its structure.

Algorithm:
1. Coarse-grain data at scales λ = 1, 2, 4, 8, ... (sliding window average)
2. At each scale, fit a simple parametric model (polynomial)
3. Track how parameters flow with scale
4. Fixed-point structure = scale-invariant terms (power laws, exponentials)
5. Reconstruct full expression from multi-scale structure

Why this is novel: RG is one of the most powerful conceptual tools in physics
(explains universality, phase transitions) but is essentially unused in SR.

**Experiment 5a**: Implement coarse-graining pipeline
**Experiment 5b**: Track parameter flow across scales
**Experiment 5c**: Detect power-law and exponential structure from scaling

---

### Direction 6: Expression Diffusion (most speculative, highest potential)

**Core idea**: Diffusion models (DALL-E, Stable Diffusion) learn to denoise.
Apply the same idea to expression trees.

Define a *corruption process* on expression trees:
- Randomly replace nodes with type-compatible random operations
- Gradually corrupt a valid expression into random noise

Train a *denoiser*:
- Given a partially-corrupted tree + target data, predict the original tree
- This learns the structure of "good expression space"

At inference:
- Start from a random tree
- Iteratively apply denoiser, conditioned on target data
- The denoiser "hallucinates" structure consistent with data + expression priors

Training data: synthetic (generate expressions, evaluate on points, use as
training pairs). Unlimited training data available.

This would be the first *generative model for symbolic expressions*.

**Experiment 6a**: Define corruption process for PTree
**Experiment 6b**: Generate synthetic training dataset
**Experiment 6c**: Train a small transformer-based denoiser
**Experiment 6d**: Test on standard SR benchmarks

---

## Unified Method: Characterize-First SR

The most promising near-term research program combines Directions 1+2+3:

```
PHASE 1 — Characterize the data
  a. Detect symmetries (parity, scaling, periodicity, Lie)
  b. Apply differential operator cascade (detect ODE)
  c. Multi-scale analysis (detect scaling behavior)
  → Output: constrained grammar / expression class

PHASE 2 — Guided symbolic search
  a. Use constrained grammar from Phase 1
  b. Symbolic residual boosting within constrained space
  c. Each boost stage: parallel tempering on constrained grammar
  → Output: sum of simple constrained expressions

PHASE 3 — Reconstruction and simplification
  a. Algebraic simplification of boosted sum
  b. Verification against ODE structure from Phase 1
  c. Constant optimization (existing: coordinate descent + golden section)
  → Output: simplified, verified symbolic expression
```

This is fundamentally different from all existing methods:
- Unlike GP: doesn't search blindly — characterizes first
- Unlike SINDy: not limited to linear combinations of a fixed basis
- Unlike neural SR: fully interpretable, no training required
- Unlike AI Feynman: works without dimensional analysis info

---

## Research Questions (Ordered by Priority)

1. **How much does symmetry detection reduce the search space?** (measurable)
2. **Can ODE discovery handle noisy data?** (practical concern)
3. **Does symbolic boosting converge? How fast?** (theoretical question)
4. **What's the right "simplicity" metric for the boosting weak learner?**
5. **Can DAGs + sharing discover known physical constants?**
6. **Is expression diffusion trainable with synthetic data?**

---

## Immediate Next Steps (Next Session)

### Session A: Symmetry Detection
1. Implement `detectSymmetries :: V.Vector (Double, Double) -> [Symmetry]`
2. Test on: sin(x), x^2, exp(x), x*sin(x), 1/x
3. Constrain PTree grammar based on detected symmetries

### Session B: Differential Operator Inversion
1. Implement numerical differentiation with noise handling
2. Implement `findLinearODE :: V.Vector (Double, Double) -> ODE`
3. Test: does the method find f'' + f = 0 for sin data?
4. Implement ODE → expression reconstruction

### Session C: Symbolic Boosting
1. Implement exhaustive search for size ≤ 5 expressions
2. Implement boosting loop
3. Test on: complex functions that GP struggles with

---

## Benchmarks for Evaluation

Standard SR benchmarks to test against:
- Feynman Symbolic Regression Benchmark (100 physics equations)
- Nguyen benchmark (standard GP test suite)
- Our own: curves-data from pinheiroTech project

Metrics:
- Recovery rate: fraction of exact equations found
- Sample efficiency: function evaluations to find exact solution
- Expression complexity: size of found expression
- Noise robustness: performance under additive Gaussian noise

Baselines:
- Our current: parallel tempering + constant optimization (main-poly.hs)
- PySR / SymbolicRegression.jl
- SINDy
- AI Feynman

---

## Wild Cards (Long-term)

- **Galois theory for SR**: the symmetry group of the expression over a function
  field. Functions with "simple" Galois groups are discoverable; "complex" ones
  are not. This might give a fundamental complexity-theoretic bound on SR.

- **Quantum annealing for SR**: map expression search to an Ising model, use
  quantum tunneling to escape local minima. IBM/D-Wave could potentially run this.

- **SR as constraint propagation**: encode the data as constraints in a symbolic
  algebra system, propagate until only one expression class is consistent.

- **SR for differential geometry**: instead of fitting f(x), fit the *manifold*
  that the data lies on. Symbolic representation of manifolds via embedding equations.
