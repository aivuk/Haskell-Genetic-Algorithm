# Hamiltonian Discovery — Typed Symbolic Regression Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `main-hamilton.hs` — a self-contained typed SR engine that discovers Hamiltonian functions from trajectory data via a symplectic residual loss, covering three physical systems in series (harmonic oscillator, rigid body, two-body gravity), then create a ralph-loop research prompt to run the experiment autonomously.

**Architecture:** One new Haskell executable (`genetic-algorithm-hamilton`) copies the `PTree`/parallel-tempering machinery from `main-mocap.hs` verbatim, then adds per-system input types, finite-difference symplectic loss functions, and Python data generators. A `HAMILTON_PROMPT.md` drives the autonomous research loop through three phases.

**Tech Stack:** Haskell (GHC 9.4, cabal), `linear` (Vec3, cross product), `async` (parallel chains), `mersenne-random`, Python 3 + NumPy (data generators), ralph-loop for autonomous iteration.

---

## File Map

| File | Action | Purpose |
|------|--------|---------|
| `genetic-algorithm.cabal` | Modify | Add `genetic-algorithm-hamilton` executable stanza |
| `main-hamilton.hs` | Create | SR engine: PTree machinery + 3 systems + dispatch |
| `scripts/generate_harmonic.py` | Create | Harmonic oscillator trajectory data (RK4) |
| `scripts/generate_rigidbody.py` | Create | Rigid body trajectory data (Euler equations) |
| `scripts/generate_twobody.py` | Create | Two-body gravity trajectory data (Verlet) |
| `HAMILTON_PROMPT.md` | Create | Autonomous ralph-loop research prompt |
| `HAMILTON_LOG.md` | Create | Research log skeleton |
| `benchmarks/hamilton_results.csv` | Create | Results tracking skeleton |
| `article/HAMILTON_DRAFT.md` | Create | Article skeleton (6 sections) |

---

## Task 1: Add cabal executable stanza

**Files:**
- Modify: `genetic-algorithm.cabal`

- [ ] **Step 1: Add stanza after the existing `genetic-algorithm-mocap` block**

Open `genetic-algorithm.cabal` and append:

```cabal
executable genetic-algorithm-hamilton
    main-is:          main-hamilton.hs
    build-depends:    base ^>=4.17
                    , mersenne-random
                    , vector
                    , array
                    , linear
                    , async
                    , time
    hs-source-dirs:   .
    default-language: Haskell2010
    ghc-options:      -XTypeApplications -O2 -threaded -rtsopts
```

- [ ] **Step 2: Verify cabal parses cleanly**

```bash
cabal build genetic-algorithm-hamilton 2>&1 | head -5
```

Expected: `Error: [Hs-source-dirs] ...` or similar "no Main module" error — that's fine, the file doesn't exist yet. What must NOT appear: "parse error" or "unknown field".

- [ ] **Step 3: Commit**

```bash
git add genetic-algorithm.cabal
git commit -m "build: add genetic-algorithm-hamilton cabal target"
```

---

## Task 2: Create main-hamilton.hs — shared machinery

**Files:**
- Create: `main-hamilton.hs`

This task writes the file from scratch with all reusable machinery. New/adapted pieces are shown in full; verbatim copies cite exact line ranges from `main-mocap.hs`.

- [ ] **Step 1: Write the file header, pragmas, and imports**

Create `/home/aivuk/Haskell-Genetic-Algorithm/main-hamilton.hs`:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Typeable              (Typeable, eqT)
import Data.Type.Equality         ((:~:)(Refl))
import Data.Proxy                 (Proxy(..))
import Data.Maybe                 (mapMaybe)
import System.Random.Mersenne     (MTGen, random, getStdGen)
import Data.Array                 (listArray, Array, bounds, (!))
import Data.IORef
import qualified Data.Vector      as V
import Text.Printf
import Control.Monad              (forM, forM_, when, replicateM)
import Data.List                  (minimumBy)
import Data.Ord                   (comparing)
import System.IO.Unsafe           (unsafePerformIO)
import Control.Concurrent.Async   (mapConcurrently)
import Data.Time.Clock            (getCurrentTime, diffUTCTime, NominalDiffTime)
import System.IO                  (hSetBuffering, stdout, BufferMode(..))
import System.Environment         (getArgs)

import Linear.V3                  (V3(..), cross)
import Linear.Metric              (dot, norm)
import Linear.Vector              ((*^))

type Vec3 = V3 Double
```

- [ ] **Step 2: Copy PTree GADT and instances**

Copy lines 50–73 of `main-mocap.hs` verbatim (the `PTree` data declaration, `Show` instance, `showVec`, `showQuat`), BUT replace the `PConstQ` constructor and `showQuat` references — Hamilton trees never output quaternions. Use this version instead:

```haskell
------------------------------------------------------------------------
-- Polymorphic expression tree
------------------------------------------------------------------------

data PTree a b where
    PBin    :: (Typeable x, Typeable y)
            => String -> (x -> y -> b) -> PTree a x -> PTree a y -> PTree a b
    PUn     :: Typeable x
            => String -> (x -> b) -> PTree a x -> PTree a b
    PV      :: String -> (a -> b) -> PTree a b
    PConst  :: IORef Double -> PTree a Double
    PConstV :: Vec3 -> PTree a Vec3

instance Show (PTree a b) where
    show (PBin n _ l r) = "(" ++ show l ++ " " ++ n ++ " " ++ show r ++ ")"
    show (PUn  n _ u)   = n ++ "(" ++ show u ++ ")"
    show (PV   n _)     = n
    show (PConst ref)   = show (unsafePerformIO (readIORef ref))
    show (PConstV v)    = let V3 x y z = v
                          in "V3(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"
```

- [ ] **Step 3: Copy evalTree through mutatePTree**

Copy lines 75–276 of `main-mocap.hs` verbatim:
`evalTree`, `sizeP`, `collectConsts`, `hasLeaf`, `foldConstants` (remove the `Quat` branch from `foldConstants` — replace the innermost `Nothing -> case eqT @b @Quat of ...` with just `Nothing -> return t`), `simplifyTree`, `TBin`/`TUn`/`TLeaf`/`MatchedBin`/`MatchedUn`, `matchBin`/`matchUn`/`matchLeaf`, `toArr`, `randElem`, `NodeType`/`randNodeType`, `genPTree`, `mutatePTree`.

The `foldConstants` function's innermost case should be:

```haskell
foldConstants t
    | not (hasLeaf t) = do
        let dummy = error "foldConstants: impossible"
        v <- evalTree t dummy
        case eqT @b @Double of
            Just Refl -> do ref <- newIORef v; return (PConst ref)
            Nothing   -> case eqT @b @Vec3 of
                Just Refl -> return (PConstV v)
                Nothing   -> return t
    | otherwise = case t of
        PBin n f l r -> PBin n f <$> foldConstants l <*> foldConstants r
        PUn  n f u   -> PUn  n f <$> foldConstants u
        _            -> return t
```

- [ ] **Step 4: Copy search infrastructure with one generalization**

Copy `goldenSearchIO` (lines 343–354) verbatim.

Write `optimizeConsts` generalized (the mocap version was monomorphic — this one works for any output type):

```haskell
optimizeConsts :: (PTree a b -> IO Double) -> PTree a b -> IO ()
optimizeConsts energy tree = do
    let refs = collectConsts tree
    forM_ [1 .. 3 :: Int] $ \_ ->
        forM_ refs $ \ref -> do
            v0 <- readIORef ref
            let radius = max 1.0 (abs v0 * 2)
                obj v  = writeIORef ref v >> energy tree
            vOpt <- goldenSearchIO obj (v0 - radius) (v0 + radius) 40
            writeIORef ref vOpt
```

Copy `SearchConfig`, `defaultSearchConfig`, `logTemps`, `formatDiff` (lines 384–424) verbatim, then adjust `defaultSearchConfig`:

```haskell
defaultSearchConfig :: SearchConfig
defaultSearchConfig = SearchConfig
    { scMaxWallSeconds  = Just 300     -- 5 minutes per run
    , scTargetEnergy    = Just 1e-4
    , scMaxRounds       = 5000
    , scStepsPerSwap    = 200
    , scLogEvery        = 10
    , scCheckpointFile  = Just "checkpoint_hamilton.csv"
    , scCheckpointEvery = 10
    , scTemps           = logTemps 12 0.001 10.0
    , scDepth           = 4
    }
```

Copy `mcStep` and `parallelTempering` (lines 430–526) verbatim.

- [ ] **Step 5: Add new shared utilities (finite differences)**

Append after `parallelTempering`:

```haskell
------------------------------------------------------------------------
-- Safe math helpers
------------------------------------------------------------------------

safeRecip :: Double -> Double
safeRecip x = if abs x < 1e-8 then 1e8 else 1 / x

safeSqrt :: Double -> Double
safeSqrt x = sqrt (abs x + 1e-12)

------------------------------------------------------------------------
-- Finite-difference gradient helpers
------------------------------------------------------------------------

-- Central-difference partial derivative of H w.r.t. one scalar input field.
partialH :: PTree inp Double
         -> (inp -> Double)         -- getter
         -> (inp -> Double -> inp)  -- setter
         -> Double                  -- epsilon
         -> inp -> IO Double
partialH tree get set eps pt = do
    let xp = set pt (get pt + eps)
        xm = set pt (get pt - eps)
    hp <- evalTree tree xp
    hm <- evalTree tree xm
    return ((hp - hm) / (2 * eps))

-- Perturb component i (0=x,1=y,2=z) of a Vec3 by eps.
perturbV3 :: Int -> Double -> Vec3 -> Vec3
perturbV3 0 eps (V3 x y z) = V3 (x + eps) y z
perturbV3 1 eps (V3 x y z) = V3 x (y + eps) z
perturbV3 2 eps (V3 x y z) = V3 x y (z + eps)
perturbV3 _ _   v          = v

-- Central-difference gradient of H w.r.t. a Vec3 input field.
gradHV3 :: PTree inp Double
        -> (inp -> Vec3)         -- getter
        -> (inp -> Vec3 -> inp)  -- setter
        -> Double -> inp -> IO Vec3
gradHV3 tree get set eps pt = do
    let comp i = do
            let hp_pt = set pt (perturbV3 i   eps  (get pt))
                hm_pt = set pt (perturbV3 i (-eps) (get pt))
            vp <- evalTree tree hp_pt
            vm <- evalTree tree hm_pt
            return ((vp - vm) / (2 * eps))
    dx <- comp 0
    dy <- comp 1
    dz <- comp 2
    return (V3 dx dy dz)

------------------------------------------------------------------------
-- CSV helper
------------------------------------------------------------------------

splitOn :: Char -> String -> [String]
splitOn delim = foldr f [""]
  where
    f c (x:xs) | c == delim = "" : x : xs
               | otherwise  = (c : x) : xs
    f _ [] = []
```

- [ ] **Step 6: Add a placeholder main so the file compiles**

```haskell
main :: IO ()
main = putStrLn "hamilton: no system selected"
```

- [ ] **Step 7: Build to verify the machinery compiles**

```bash
cabal build genetic-algorithm-hamilton 2>&1 | tail -5
```

Expected: `Linking .../genetic-algorithm-hamilton ...` (success).

- [ ] **Step 8: Commit**

```bash
git add main-hamilton.hs
git commit -m "feat: add main-hamilton.hs with shared PTree/tempering machinery"
```

---

## Task 3: System (a) — 1D Harmonic Oscillator

**Files:**
- Modify: `main-hamilton.hs` (add system a before `main`)
- Create: `scripts/generate_harmonic.py`

- [ ] **Step 1: Write the harmonic data generator**

Create `scripts/generate_harmonic.py`:

```python
#!/usr/bin/env python3
"""
Generate harmonic oscillator trajectory data.
Integrates q'' = -k*q (i.e. q'=p, p'=-k*q) using RK4.
Outputs: q, p, dq_dt, dp_dt
k varies per trajectory so the tree cannot absorb it as a constant.
"""
import sys
import numpy as np

def rk4_step(state, k, dt):
    q, p = state
    def deriv(q, p):
        return p, -k * q
    dq1, dp1 = deriv(q, p)
    dq2, dp2 = deriv(q + 0.5*dt*dq1, p + 0.5*dt*dp1)
    dq3, dp3 = deriv(q + 0.5*dt*dq2, p + 0.5*dt*dp2)
    dq4, dp4 = deriv(q + dt*dq3, p + dt*dp3)
    return (q + dt*(dq1 + 2*dq2 + 2*dq3 + dq4)/6,
            p + dt*(dp1 + 2*dp2 + 2*dp3 + dp4)/6)

def main():
    out_path = sys.argv[1] if len(sys.argv) > 1 else "data/harmonic.csv"
    rng = np.random.default_rng(42)
    n_traj = 50
    n_steps = 60
    dt = 0.05

    rows = []
    for _ in range(n_traj):
        k = rng.uniform(3.8, 4.2)
        q, p = rng.uniform(-2.0, 2.0), rng.uniform(-2.0, 2.0)
        for _ in range(n_steps):
            dq_dt = p
            dp_dt = -k * q
            rows.append((q, p, dq_dt, dp_dt))
            q, p = rk4_step((q, p), k, dt)

    with open(out_path, 'w') as f:
        f.write("q,p,dq_dt,dp_dt\n")
        for row in rows:
            f.write(",".join(f"{v:.10f}" for v in row) + "\n")

    print(f"Written {len(rows)} rows to {out_path}")

if __name__ == "__main__":
    main()
```

- [ ] **Step 2: Generate the data and verify**

```bash
/home/aivuk/.venv-mocap/bin/python3 scripts/generate_harmonic.py data/harmonic.csv
head -3 data/harmonic.csv
```

Expected output:
```
Written 3000 rows to data/harmonic.csv
q,p,dq_dt,dp_dt
<numbers>,<numbers>,<numbers>,<numbers>
```

- [ ] **Step 3: Add system (a) Haskell code to main-hamilton.hs**

Insert before the placeholder `main`:

```haskell
------------------------------------------------------------------------
-- System (a): 1D Harmonic Oscillator
------------------------------------------------------------------------

data HarmonicInput = HarmonicInput
    { hQ    :: Double
    , hP    :: Double
    , hDotQ :: Double   -- observed dq/dt (loss only, not a tree leaf)
    , hDotP :: Double   -- observed dp/dt (loss only, not a tree leaf)
    } deriving (Show)

loadHarmonicCSV :: FilePath -> IO (V.Vector HarmonicInput)
loadHarmonicCSV path = do
    content <- readFile path
    let ls = drop 1 (lines content)
        parse l = case map (read :: String -> Double) (splitOn ',' l) of
            [q, p, dq, dp] -> Just (HarmonicInput q p dq dp)
            _              -> Nothing
    return $ V.fromList $ mapMaybe parse ls

harmonicBins :: [TBin]
harmonicBins =
    [ TBin "(+)" ((+) :: Double -> Double -> Double)
    , TBin "(*)" ((*) :: Double -> Double -> Double)
    , TBin "(-)" ((-) :: Double -> Double -> Double)
    ]

harmonicUns :: [TUn]
harmonicUns =
    [ TUn "sq"        (\x -> x * x  :: Double)
    , TUn "neg"       (negate        :: Double -> Double)
    , TUn "safeRecip" (safeRecip     :: Double -> Double)
    ]

harmonicLeaves :: [TLeaf HarmonicInput]
harmonicLeaves =
    [ TLeaf "q" (hQ :: HarmonicInput -> Double)
    , TLeaf "p" (hP :: HarmonicInput -> Double)
    ]

sympLossHarmonic :: V.Vector HarmonicInput
                 -> PTree HarmonicInput Double -> IO Double
sympLossHarmonic pts tree = do
    errs <- V.forM pts $ \pt -> do
        dHdq <- partialH tree hQ (\r x -> r { hQ = x }) 1e-5 pt
        dHdp <- partialH tree hP (\r x -> r { hP = x }) 1e-5 pt
        let eq = hDotQ pt - dHdp   -- Hamilton: q' = dH/dp
            ep = hDotP pt + dHdq   -- Hamilton: p' = -dH/dq
        return (eq*eq + ep*ep)
    return $ V.sum errs / fromIntegral (V.length errs)

runHarmonic :: IO ()
runHarmonic = do
    putStrLn "=== System (a): 1D Harmonic Oscillator ==="
    allPts <- loadHarmonicCSV "data/harmonic.csv"
    let pts = V.take 300 allPts
    printf "Loaded %d rows, using %d for search\n" (V.length allPts) (V.length pts)

    let ef tree = sympLossHarmonic pts tree
        cfg = defaultSearchConfig { scTargetEnergy = Just 1e-4, scDepth = 4 }

    printf "Starting search: %d chains, max %d rounds, depth %d\n"
        (length (scTemps cfg)) (scMaxRounds cfg) (scDepth cfg)

    (e, best, reason) <- parallelTempering @HarmonicInput @Double
                             harmonicBins harmonicUns harmonicLeaves ef cfg

    optimizeConsts (sympLossHarmonic allPts) best
    eFinal <- sympLossHarmonic allPts best
    simplified <- foldConstants best
    normalized <- simplifyTree simplified

    putStrLn $ "\nStopped: " ++ reason
    printf "Search loss (300 pts):    %.3e\n" e
    printf "Final loss (all %d pts): %.3e\n" (V.length allPts) eFinal
    putStrLn $ "Best tree (raw):        " ++ show best
    putStrLn $ "Best tree (normalized): " ++ show normalized

    putStrLn "\nSample predictions (first 5 rows):"
    putStrLn "q            p            dH/dp(found) dq/dt(obs)"
    V.forM_ (V.take 5 pts) $ \pt -> do
        dHdp <- partialH best hP (\r x -> r { hP = x }) 1e-5 pt
        printf "%-12.4f %-12.4f %-12.4f %-12.4f\n"
            (hQ pt) (hP pt) dHdp (hDotQ pt)
```

- [ ] **Step 4: Wire system (a) into main**

Replace the placeholder `main` with:

```haskell
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case args of
        ["harmonic"]  -> runHarmonic
        ["rigidbody"] -> putStrLn "rigidbody: not yet implemented"
        ["nbody"]     -> putStrLn "nbody: not yet implemented"
        _             -> putStrLn "Usage: genetic-algorithm-hamilton [harmonic|rigidbody|nbody]"
```

- [ ] **Step 5: Build**

```bash
cabal build genetic-algorithm-hamilton 2>&1 | tail -5
```

Expected: `Linking .../genetic-algorithm-hamilton ...`

- [ ] **Step 6: Smoke-test system (a)**

```bash
timeout 60 cabal run genetic-algorithm-hamilton -- harmonic +RTS -N4 2>&1 | head -20
```

Expected: search starts, prints `[round 10/5000 ...]`, loss decreasing. May or may not reach target in 60s — that's fine.

- [ ] **Step 7: Commit**

```bash
git add main-hamilton.hs scripts/generate_harmonic.py data/harmonic.csv
git commit -m "feat: add system (a) harmonic oscillator with symplectic residual loss"
```

---

## Task 4: System (b) — 3D Rigid Body

**Files:**
- Create: `scripts/generate_rigidbody.py`
- Modify: `main-hamilton.hs`

- [ ] **Step 1: Write the rigid body data generator**

Create `scripts/generate_rigidbody.py`:

```python
#!/usr/bin/env python3
"""
Generate rigid body (torque-free) trajectory data.
Euler equations: dL/dt = L x Omega, where Omega = I_inv * L.
I = diag(1.0, 2.0, 3.0)  =>  I_inv = diag(1.0, 0.5, 1/3)
Outputs: lx, ly, lz, dlx_dt, dly_dt, dlz_dt
"""
import sys
import numpy as np

I_inv = np.array([1.0, 0.5, 1.0/3.0])

def deriv(L):
    Omega = I_inv * L
    return np.cross(L, Omega)

def rk4_step(L, dt):
    k1 = deriv(L)
    k2 = deriv(L + 0.5*dt*k1)
    k3 = deriv(L + 0.5*dt*k2)
    k4 = deriv(L + dt*k3)
    return L + dt*(k1 + 2*k2 + 2*k3 + k4)/6

def main():
    out_path = sys.argv[1] if len(sys.argv) > 1 else "data/rigidbody.csv"
    rng = np.random.default_rng(7)
    n_traj = 50
    n_steps = 60
    dt = 0.02

    rows = []
    for _ in range(n_traj):
        # Random initial L on a sphere of random radius [0.5, 3.0]
        r = rng.uniform(0.5, 3.0)
        L = rng.normal(size=3)
        L = L / np.linalg.norm(L) * r
        for _ in range(n_steps):
            dL = deriv(L)
            rows.append((*L, *dL))
            L = rk4_step(L, dt)

    with open(out_path, 'w') as f:
        f.write("lx,ly,lz,dlx_dt,dly_dt,dlz_dt\n")
        for row in rows:
            f.write(",".join(f"{v:.10f}" for v in row) + "\n")

    print(f"Written {len(rows)} rows to {out_path}")

    # Validate: check that dL/dt ≈ L × (I_inv * L) for first row
    L0 = np.array(rows[0][:3])
    dL0_obs = np.array(rows[0][3:])
    dL0_pred = deriv(L0)
    err = np.linalg.norm(dL0_obs - dL0_pred)
    print(f"Validation residual (first row): {err:.2e}  (should be ~0)")

if __name__ == "__main__":
    main()
```

- [ ] **Step 2: Generate the data**

```bash
/home/aivuk/.venv-mocap/bin/python3 scripts/generate_rigidbody.py data/rigidbody.csv
```

Expected:
```
Written 3000 rows to data/rigidbody.csv
Validation residual (first row): <very small number>
```

- [ ] **Step 3: Add system (b) Haskell code to main-hamilton.hs**

Insert before `main`:

```haskell
------------------------------------------------------------------------
-- System (b): 3D Rigid Body  (I = diag(1,2,3), H = Lx²/2 + Ly²/4 + Lz²/6)
------------------------------------------------------------------------

data RigidBodyInput = RigidBodyInput
    { rbL    :: Vec3   -- angular momentum (tree input)
    , rbDotL :: Vec3   -- observed dL/dt   (loss only)
    } deriving (Show)

loadRigidBodyCSV :: FilePath -> IO (V.Vector RigidBodyInput)
loadRigidBodyCSV path = do
    content <- readFile path
    let ls = drop 1 (lines content)
        parse l = case map (read :: String -> Double) (splitOn ',' l) of
            [lx,ly,lz,dlx,dly,dlz] ->
                Just (RigidBodyInput (V3 lx ly lz) (V3 dlx dly dlz))
            _ -> Nothing
    return $ V.fromList $ mapMaybe parse ls

-- Leaves: project each component of L individually
rigidBodyBins :: [TBin]
rigidBodyBins =
    [ TBin "(+)" ((+) :: Double -> Double -> Double)
    , TBin "(*)" ((*) :: Double -> Double -> Double)
    ]

rigidBodyUns :: [TUn]
rigidBodyUns =
    [ TUn "sq"        (\x -> x * x :: Double)
    , TUn "neg"       (negate       :: Double -> Double)
    , TUn "safeRecip" (safeRecip    :: Double -> Double)
    ]

rigidBodyLeaves :: [TLeaf RigidBodyInput]
rigidBodyLeaves =
    [ TLeaf "lx" (\r -> let V3 x _ _ = rbL r in x)
    , TLeaf "ly" (\r -> let V3 _ y _ = rbL r in y)
    , TLeaf "lz" (\r -> let V3 _ _ z = rbL r in z)
    ]

sympLossRigidBody :: V.Vector RigidBodyInput
                  -> PTree RigidBodyInput Double -> IO Double
sympLossRigidBody pts tree = do
    errs <- V.forM pts $ \pt -> do
        -- grad H w.r.t. L via finite differences on each component
        gradH <- gradHV3 tree rbL (\r v -> r { rbL = v }) 1e-5 pt
        -- Euler equation: dL/dt = L × (∂H/∂L)
        let predicted = rbL pt `cross` gradH
            residual  = rbDotL pt - predicted
        return (dot residual residual)
    return $ V.sum errs / fromIntegral (V.length errs)

runRigidBody :: IO ()
runRigidBody = do
    putStrLn "=== System (b): 3D Rigid Body ==="
    allPts <- loadRigidBodyCSV "data/rigidbody.csv"
    let pts = V.take 300 allPts
    printf "Loaded %d rows, using %d for search\n" (V.length allPts) (V.length pts)

    let ef tree = sympLossRigidBody pts tree
        cfg = defaultSearchConfig { scTargetEnergy = Just 1e-4, scDepth = 4 }

    printf "Starting search: %d chains, max %d rounds, depth %d\n"
        (length (scTemps cfg)) (scMaxRounds cfg) (scDepth cfg)

    (e, best, reason) <- parallelTempering @RigidBodyInput @Double
                             rigidBodyBins rigidBodyUns rigidBodyLeaves ef cfg

    optimizeConsts (sympLossRigidBody allPts) best
    eFinal <- sympLossRigidBody allPts best
    simplified <- foldConstants best
    normalized <- simplifyTree simplified

    putStrLn $ "\nStopped: " ++ reason
    printf "Search loss (300 pts):    %.3e\n" e
    printf "Final loss (all %d pts): %.3e\n" (V.length allPts) eFinal
    putStrLn $ "Best tree (raw):        " ++ show best
    putStrLn $ "Best tree (normalized): " ++ show normalized
```

- [ ] **Step 4: Wire system (b) into main**

Replace `["rigidbody"] -> putStrLn "rigidbody: not yet implemented"` with:

```haskell
        ["rigidbody"] -> runRigidBody
```

- [ ] **Step 5: Build**

```bash
cabal build genetic-algorithm-hamilton 2>&1 | tail -5
```

Expected: `Linking .../genetic-algorithm-hamilton ...`

- [ ] **Step 6: Smoke-test system (b)**

```bash
timeout 60 cabal run genetic-algorithm-hamilton -- rigidbody +RTS -N4 2>&1 | head -20
```

Expected: search starts, loss decreasing.

- [ ] **Step 7: Commit**

```bash
git add main-hamilton.hs scripts/generate_rigidbody.py data/rigidbody.csv
git commit -m "feat: add system (b) rigid body with Vec3 symplectic loss"
```

---

## Task 5: System (c) — Two-Body Gravity

**Files:**
- Create: `scripts/generate_twobody.py`
- Modify: `main-hamilton.hs`

- [ ] **Step 1: Write the two-body data generator**

Create `scripts/generate_twobody.py`:

```python
#!/usr/bin/env python3
"""
Generate two-body gravitational trajectory data (G=1, m1=m2=1).
H = (|p1|^2 + |p2|^2)/2 - 1/|q1-q2|
Uses velocity Verlet (symplectic) integration.
Outputs: q1x,q1y,q1z,q2x,q2y,q2z,p1x,p1y,p1z,p2x,p2y,p2z,
         dq1x,...,dp2z  (24 columns)
"""
import sys
import numpy as np

def force(q1, q2):
    r = q2 - q1
    d = np.linalg.norm(r)
    return r / (d**3 + 1e-10)   # F on body 1 from body 2

def deriv(q1, q2, p1, p2):
    f = force(q1, q2)
    return p1, p2, f, -f   # dq1, dq2, dp1, dp2

def rk4_step(q1, q2, p1, p2, dt):
    k1 = deriv(q1, q2, p1, p2)
    def s(a, k, h): return a + h*k
    k2 = deriv(s(q1,k1[0],dt/2), s(q2,k1[1],dt/2),
               s(p1,k1[2],dt/2), s(p2,k1[3],dt/2))
    k3 = deriv(s(q1,k2[0],dt/2), s(q2,k2[1],dt/2),
               s(p1,k2[2],dt/2), s(p2,k2[3],dt/2))
    k4 = deriv(s(q1,k3[0],dt), s(q2,k3[1],dt),
               s(p1,k3[2],dt), s(p2,k3[3],dt))
    def combine(a, k1i, k2i, k3i, k4i):
        return a + dt*(k1i + 2*k2i + 2*k3i + k4i)/6
    return (combine(q1,k1[0],k2[0],k3[0],k4[0]),
            combine(q2,k1[1],k2[1],k3[1],k4[1]),
            combine(p1,k1[2],k2[2],k3[2],k4[2]),
            combine(p2,k1[3],k2[3],k3[3],k4[3]))

def main():
    out_path = sys.argv[1] if len(sys.argv) > 1 else "data/twobody.csv"
    rng = np.random.default_rng(13)
    n_traj = 50
    n_steps = 60
    dt = 0.01

    rows = []
    for _ in range(n_traj):
        # Random bound initial conditions (total energy < 0)
        while True:
            q1 = rng.uniform(-1.5, 1.5, 3)
            q2 = rng.uniform(-1.5, 1.5, 3)
            p1 = rng.uniform(-1.0, 1.0, 3)
            p2 = -p1  # zero total momentum
            d = np.linalg.norm(q2 - q1)
            if d > 0.3:   # avoid near-singularity
                KE = 0.5 * (np.dot(p1,p1) + np.dot(p2,p2))
                PE = -1.0 / d
                if KE + PE < -0.1:  # bound orbit
                    break

        for _ in range(n_steps):
            dq1, dq2, dp1, dp2 = deriv(q1, q2, p1, p2)
            rows.append((*q1, *q2, *p1, *p2, *dq1, *dq2, *dp1, *dp2))
            q1, q2, p1, p2 = rk4_step(q1, q2, p1, p2, dt)

    cols = ["q1x","q1y","q1z","q2x","q2y","q2z",
            "p1x","p1y","p1z","p2x","p2y","p2z",
            "dq1x","dq1y","dq1z","dq2x","dq2y","dq2z",
            "dp1x","dp1y","dp1z","dp2x","dp2y","dp2z"]
    with open(out_path, 'w') as f:
        f.write(",".join(cols) + "\n")
        for row in rows:
            f.write(",".join(f"{v:.10f}" for v in row) + "\n")

    print(f"Written {len(rows)} rows to {out_path}")

if __name__ == "__main__":
    main()
```

- [ ] **Step 2: Generate two-body data**

```bash
/home/aivuk/.venv-mocap/bin/python3 scripts/generate_twobody.py data/twobody.csv
```

Expected: `Written 3000 rows to data/twobody.csv`

- [ ] **Step 3: Add system (c) Haskell code to main-hamilton.hs**

Insert before `main`:

```haskell
------------------------------------------------------------------------
-- System (c): Two-Body Gravity  (G=1, m=1, H = (|p1|²+|p2|²)/2 - 1/|q1-q2|)
------------------------------------------------------------------------

data TwoBodyInput = TwoBodyInput
    { tbQ1    :: Vec3, tbQ2    :: Vec3   -- positions (tree inputs)
    , tbP1    :: Vec3, tbP2    :: Vec3   -- momenta   (tree inputs)
    , tbDotQ1 :: Vec3, tbDotQ2 :: Vec3   -- dq/dt     (loss only)
    , tbDotP1 :: Vec3, tbDotP2 :: Vec3   -- dp/dt     (loss only)
    } deriving (Show)

loadTwoBodyCSV :: FilePath -> IO (V.Vector TwoBodyInput)
loadTwoBodyCSV path = do
    content <- readFile path
    let ls = drop 1 (lines content)
        parse l = case map (read :: String -> Double) (splitOn ',' l) of
            [q1x,q1y,q1z, q2x,q2y,q2z,
             p1x,p1y,p1z, p2x,p2y,p2z,
             dq1x,dq1y,dq1z, dq2x,dq2y,dq2z,
             dp1x,dp1y,dp1z, dp2x,dp2y,dp2z] ->
                Just (TwoBodyInput
                    (V3 q1x q1y q1z) (V3 q2x q2y q2z)
                    (V3 p1x p1y p1z) (V3 p2x p2y p2z)
                    (V3 dq1x dq1y dq1z) (V3 dq2x dq2y dq2z)
                    (V3 dp1x dp1y dp1z) (V3 dp2x dp2y dp2z))
            _ -> Nothing
    return $ V.fromList $ mapMaybe parse ls

twoBodyBins :: [TBin]
twoBodyBins =
    [ TBin "(+)"   ((+)    :: Double -> Double -> Double)
    , TBin "(*)"   ((*) :: Double -> Double -> Double)
    , TBin "(-)"   ((-) :: Double -> Double -> Double)
    , TBin "dot_v" (dot    :: Vec3 -> Vec3 -> Double)
    , TBin "sub_v" ((-) :: Vec3 -> Vec3 -> Vec3)
    ]

twoBodyUns :: [TUn]
twoBodyUns =
    [ TUn "sq"        (\x -> x * x :: Double)
    , TUn "neg"       (negate        :: Double -> Double)
    , TUn "safeRecip" (safeRecip     :: Double -> Double)
    , TUn "safeSqrt"  (safeSqrt      :: Double -> Double)
    ]

twoBodyLeaves :: [TLeaf TwoBodyInput]
twoBodyLeaves =
    [ TLeaf "q1" (tbQ1 :: TwoBodyInput -> Vec3)
    , TLeaf "q2" (tbQ2 :: TwoBodyInput -> Vec3)
    , TLeaf "p1" (tbP1 :: TwoBodyInput -> Vec3)
    , TLeaf "p2" (tbP2 :: TwoBodyInput -> Vec3)
    ]

sympLossTwoBody :: V.Vector TwoBodyInput
                -> PTree TwoBodyInput Double -> IO Double
sympLossTwoBody pts tree = do
    errs <- V.forM pts $ \pt -> do
        -- 24 evalTree calls: 6 per Vec3 field × 4 fields
        gq1 <- gradHV3 tree tbQ1 (\r v -> r { tbQ1 = v }) 1e-5 pt
        gq2 <- gradHV3 tree tbQ2 (\r v -> r { tbQ2 = v }) 1e-5 pt
        gp1 <- gradHV3 tree tbP1 (\r v -> r { tbP1 = v }) 1e-5 pt
        gp2 <- gradHV3 tree tbP2 (\r v -> r { tbP2 = v }) 1e-5 pt
        -- Hamilton: dq/dt = ∂H/∂p,   dp/dt = -∂H/∂q
        let rq1 = tbDotQ1 pt - gp1
            rq2 = tbDotQ2 pt - gp2
            rp1 = tbDotP1 pt + gq1
            rp2 = tbDotP2 pt + gq2
        return (dot rq1 rq1 + dot rq2 rq2 + dot rp1 rp1 + dot rp2 rp2)
    return $ V.sum errs / fromIntegral (V.length errs)

runTwoBody :: IO ()
runTwoBody = do
    putStrLn "=== System (c): Two-Body Gravity ==="
    allPts <- loadTwoBodyCSV "data/twobody.csv"
    let pts = V.take 300 allPts
    printf "Loaded %d rows, using %d for search\n" (V.length allPts) (V.length pts)

    let ef tree = sympLossTwoBody pts tree
        cfg = defaultSearchConfig
                { scTargetEnergy = Just 1e-3
                , scDepth        = 5
                }

    printf "Starting search: %d chains, max %d rounds, depth %d\n"
        (length (scTemps cfg)) (scMaxRounds cfg) (scDepth cfg)

    (e, best, reason) <- parallelTempering @TwoBodyInput @Double
                             twoBodyBins twoBodyUns twoBodyLeaves ef cfg

    optimizeConsts (sympLossTwoBody allPts) best
    eFinal <- sympLossTwoBody allPts best
    simplified <- foldConstants best
    normalized <- simplifyTree simplified

    putStrLn $ "\nStopped: " ++ reason
    printf "Search loss (300 pts):    %.3e\n" e
    printf "Final loss (all %d pts): %.3e\n" (V.length allPts) eFinal
    putStrLn $ "Best tree (raw):        " ++ show best
    putStrLn $ "Best tree (normalized): " ++ show normalized
```

- [ ] **Step 4: Wire system (c) into main**

Replace `["nbody"] -> putStrLn "nbody: not yet implemented"` with:

```haskell
        ["nbody"]     -> runTwoBody
```

- [ ] **Step 5: Build**

```bash
cabal build genetic-algorithm-hamilton 2>&1 | tail -5
```

Expected: `Linking .../genetic-algorithm-hamilton ...`

- [ ] **Step 6: Smoke-test system (c)**

```bash
timeout 60 cabal run genetic-algorithm-hamilton -- nbody +RTS -N4 2>&1 | head -20
```

Expected: search starts, loss values printed, no NaN/Infinity.

- [ ] **Step 7: Commit**

```bash
git add main-hamilton.hs scripts/generate_twobody.py data/twobody.csv
git commit -m "feat: add system (c) two-body gravity with Vec3 symplectic loss"
```

---

## Task 6: Create HAMILTON_PROMPT.md and tracking file skeletons

**Files:**
- Create: `HAMILTON_PROMPT.md`
- Create: `HAMILTON_LOG.md`
- Create: `benchmarks/hamilton_results.csv`
- Create: `article/HAMILTON_DRAFT.md`

- [ ] **Step 1: Write HAMILTON_PROMPT.md**

Create `/home/aivuk/Haskell-Genetic-Algorithm/HAMILTON_PROMPT.md`:

````markdown
# Autonomous Research Loop: Hamiltonian Discovery via Typed SR

## Project
Working directory: /home/aivuk/Haskell-Genetic-Algorithm
Goal: discover Hamiltonian functions H(q,p) for three physical systems in series
using a typed GADT SR system with symplectic residual loss.

Systems (in order):
1. **(a) Harmonic oscillator** — `H = 0.5·p² + 0.5·k·q²`  (k≈4)
2. **(b) 3D Rigid body**       — `H = Lx²/2 + Ly²/4 + Lz²/6`
3. **(c) Two-body gravity**    — `H = (|p1|²+|p2|²)/2 − 1/|q1−q2|`

## Permissions granted — do all of the following without asking:
- Edit any file in /home/aivuk/Haskell-Genetic-Algorithm/
- Run `cabal build`, `cabal run`, `python3`, shell commands
- Commit to git (small focused commits after each change)
- Write/append to HAMILTON_LOG.md, benchmarks/hamilton_results.csv, article/HAMILTON_DRAFT.md

## Phase tracking
Read HAMILTON_LOG.md to determine current phase and iteration number.
- If no iteration entries exist → start Phase (a), Iteration 1
- If Phase (a) success criterion met (≥8/10) → continue with Phase (b)
- If Phase (b) success criterion met → continue with Phase (c)
- If Phase (c) success criterion met → check all stopping conditions

## Key files
- `main-hamilton.hs` — the SR engine
- `scripts/generate_harmonic.py` — phase (a) data generator
- `scripts/generate_rigidbody.py` — phase (b) data generator
- `scripts/generate_twobody.py`  — phase (c) data generator
- `HAMILTON_LOG.md` — running research log
- `benchmarks/hamilton_results.csv` — quantitative results
- `article/HAMILTON_DRAFT.md` — the article

---

## Each Iteration — follow this procedure exactly

### Step 1: Regenerate data for current phase
```bash
# Phase (a):
/home/aivuk/.venv-mocap/bin/python3 scripts/generate_harmonic.py data/harmonic.csv
# Phase (b):
/home/aivuk/.venv-mocap/bin/python3 scripts/generate_rigidbody.py data/rigidbody.csv
# Phase (c):
/home/aivuk/.venv-mocap/bin/python3 scripts/generate_twobody.py data/twobody.csv
```

### Step 2: Run a timed search (3 runs)
```bash
# Phase (a):
timeout 300 cabal run genetic-algorithm-hamilton -- harmonic -- +RTS -N4
# Phase (b):
timeout 300 cabal run genetic-algorithm-hamilton -- rigidbody -- +RTS -N4
# Phase (c):
timeout 300 cabal run genetic-algorithm-hamilton -- nbody -- +RTS -N4
```
Run 3 times. Capture: loss printed, best tree string, stopped reason.

### Step 3: Analyse each tree mathematically
For each tree found:
a. Simplify algebraically step by step
b. Classify failure mode using HAMILTON_LOG.md failure mode tables
c. Check: does the simplified form match the ground truth H?
   - YES → success for this run
   - NO  → identify which assumption broke

### Step 4: Determine the fix
Based on dominant failure mode, select ONE targeted fix.

**Phase (a) priority order:**
1. If `p` leaf absent → verify p is in harmonicLeaves; check tree depth
2. If `q` leaf absent → same check
3. If constant structure correct but wrong coefficients → increase optimizeConsts passes from 3 to 5
4. If finding `H = c·p² only` (ignoring q) → vary initial conditions more in generate_harmonic.py
5. If finding trivial constant → increase depth from 4 to 5
6. If complex but equivalent → reduce depth from 4 to 3
7. If energy plateaus → increase scTemps chain count from 12 to 16
8. If all 3 runs succeed → run 10 runs, record success rate

**Phase (b) priority order:**
1. If only finds `c·lx²` (ignores ly,lz) → verify all three leaves in pool; vary initial L more
2. If wrong coefficients → increase optimizeConsts passes
3. If complex structure → reduce depth
4. If energy plateaus → increase chain count
5. If all 3 runs succeed → run 10 runs, record success rate

**Phase (c) priority order:**
1. If NaN/Infinity in loss → check safeRecip/safeSqrt are guarding correctly
2. If finds kinetic term only → verify safeRecip in pool enables 1/r discovery
3. If finds only `1/|q1-q2|` without kinetic → increase depth
4. If constants wrong → increase optimizeConsts passes
5. If energy plateaus → reduce search points from 300 to 150
6. If all 3 runs succeed → run 10 runs, record success rate

**After Phase (a) reaches 8/10 — Approach B upgrade (one iteration):**
Add `diffTree` symbolic differentiation to `main-hamilton.hs` for the harmonic
system. Replace `partialH` call in `sympLossHarmonic` with `evalTree (diffTree tree "p")`.
Compare residual accuracy and speed. Document in article section 2.

Only make ONE change per iteration.

### Step 5: Implement the fix
Edit the relevant file. Build:
```bash
cabal build genetic-algorithm-hamilton 2>&1 | tail -5
```
Fix build errors before proceeding.

### Step 6: Record results
Append to benchmarks/hamilton_results.csv:
```
<phase>,<iteration>,<run 1-3>,<loss_search>,<loss_full>,<uses_correct_vars T/F>,<failure_mode>,<change_made>,<tree_simplified>
```

Append to HAMILTON_LOG.md:
```markdown
### Phase (X) Iteration N — YYYY-MM-DD

**Runs:** 3
**Trees found:** [simplified form of each]
**Failure modes:** [code for each]
**Dominant failure:** [most common]
**Fix applied:** [description]
**Before/after loss:** [numbers]
**Insight:** [one sentence]
```

### Step 7: Update article draft
Append to article/HAMILTON_DRAFT.md. Sections:
1. Introduction — why Hamiltonian discovery, why typed SR
2. Method — PTree reuse, finite-diff symplectic loss, symbolic diff (Approach B)
3. System (a) — harmonic oscillator: results, failure modes found
4. System (b) — rigid body: results, how Vec3 types helped
5. System (c) — two-body gravity: results, safeRecip story
6. Conclusion — lessons, future work

Do not fabricate results. Only write what has been observed.

### Step 8: Commit
```bash
git add -A
git commit -m "research: phase (X) iter N — <one-line summary>"
```

---

## Success criteria per phase

**Phase (a) success:** tree simplifies to `c1·p² + c2·q²`
with `c1 ∈ [0.48, 0.52]`, `c2 ∈ [1.9, 2.1]`, full-dataset loss < 1e-4.

**Phase (b) success:** tree simplifies to `c1·lx² + c2·ly² + c3·lz²`
with `c1 ∈ [0.475, 0.525]`, `c2 ∈ [0.228, 0.272]`, `c3 ∈ [0.152, 0.182]`,
full-dataset loss < 1e-4.

**Phase (c) success:** tree simplifies to `c1·(dot_v p1 p1 + dot_v p2 p2) + c2·safeRecip(safeSqrt(dot_v(sub_v q1 q2)(sub_v q1 q2)))`
with `c1 ∈ [0.48, 0.52]`, `c2 ∈ [-1.05, -0.95]`, full-dataset loss < 1e-3.

---

## Stopping Conditions — emit promise when ALL are true:

1. **Convergence**: all three phases have ≥ 8/10 consecutive runs finding correct H
2. **Article**: all 6 sections written with real results, ≥ 600 words total,
   results table has ≥ 5 iterations per phase
3. **Benchmarks**: hamilton_results.csv has ≥ 20 rows, improvement trend per phase
4. **Code**: `cabal build genetic-algorithm-hamilton` compiles cleanly

When all four conditions are met, output:
```
<promise>HAMILTON RESEARCH COMPLETE</promise>
```

---

## Recovery

- Build failure → fix, rebuild before proceeding
- NaN in loss → add `isNaN`/`isInfinite` guard in the loss function, return large value
- All 3 runs timeout with no improvement → apply chain count fix or reduce search points
- Fix makes things worse → `git checkout main-hamilton.hs`, log as negative result
- Stuck on same failure for 3+ iterations → try fundamentally different approach

## Max iterations: 40 (across all phases). Track in HAMILTON_LOG.md.
````

- [ ] **Step 2: Write HAMILTON_LOG.md skeleton**

Create `/home/aivuk/Haskell-Genetic-Algorithm/HAMILTON_LOG.md`:

```markdown
# Hamiltonian Discovery — Research Log

## Problem
Discover Hamiltonian functions H(q,p) for three physical systems using
multi-typed GADT symbolic regression with symplectic residual loss.

## Ground Truths
- Phase (a): `H = 0.5·p² + 0.5·k·q²`  (k ≈ 4)
- Phase (b): `H = Lx²/2 + Ly²/4 + Lz²/6`
- Phase (c): `H = (|p1|² + |p2|²)/2 − 1/|q1−q2|`

## Success Criteria
- Phase (a): c1·p² + c2·q² with c1∈[0.48,0.52], c2∈[1.9,2.1], loss < 1e-4
- Phase (b): c1·lx² + c2·ly² + c3·lz² within 5% of 1/(2Iᵢ), loss < 1e-4
- Phase (c): kinetic + gravity terms within 5%, loss < 1e-3

## Phase (a) Failure Modes
| ID | Name | Pattern | Cause | Fix |
|----|------|---------|-------|-----|
| A1 | MissingLeaf | H uses only p (no q) | data has correlated q,p | vary ICs more |
| A2 | WrongScale | correct structure, wrong c | optimizer needs more passes | increase passes |
| A3 | TrivialConst | H = constant | tree too shallow | increase depth |
| A4 | ComplexEquiv | correct but verbose | too many operators | reduce depth |

## Phase (b) Failure Modes
| ID | Name | Pattern | Cause | Fix |
|----|------|---------|-------|-----|
| B1 | PartialL | H uses only lx | other components correlated | vary initial L |
| B2 | WrongScale | correct structure, wrong c | optimizer | increase passes |

## Phase (c) Failure Modes
| ID | Name | Pattern | Cause | Fix |
|----|------|---------|-------|-----|
| C1 | NaNLoss | loss = NaN | safeRecip/safeSqrt gap | tighten guard |
| C2 | KineticOnly | H = |p|² terms only | gravity term too complex | increase depth |
| C3 | GravityOnly | H = 1/r only | kinetic term absorbed | more varied momenta |

## Iteration Log

(populated by ralph-loop)
```

- [ ] **Step 3: Write benchmarks/hamilton_results.csv header**

```bash
echo "phase,iteration,run,loss_search,loss_full,uses_correct_vars,failure_mode,change_made,tree_simplified" > benchmarks/hamilton_results.csv
```

- [ ] **Step 4: Write article/HAMILTON_DRAFT.md skeleton**

Create `/home/aivuk/Haskell-Genetic-Algorithm/article/HAMILTON_DRAFT.md`:

```markdown
# Discovering Hamiltonians with Typed Symbolic Regression

## 1. Introduction

*(to be filled with real results)*

## 2. Method

*(to be filled with real results)*

## 3. System (a): Harmonic Oscillator

*(to be filled with real results)*

## 4. System (b): 3D Rigid Body

*(to be filled with real results)*

## 5. System (c): Two-Body Gravity

*(to be filled with real results)*

## 6. Conclusion

*(to be filled with real results)*
```

- [ ] **Step 5: Commit everything**

```bash
git add HAMILTON_PROMPT.md HAMILTON_LOG.md benchmarks/hamilton_results.csv article/HAMILTON_DRAFT.md
git commit -m "research: add Hamilton research loop infrastructure (prompt, log, article skeleton)"
```

---

## Self-Review

**Spec coverage:**
- System (a) harmonic: ✓ Task 3
- System (b) rigid body: ✓ Task 4
- System (c) two-body: ✓ Task 5
- Finite-difference loss (Approach A): ✓ Task 2 (`partialH`, `gradHV3`)
- Approach B (symbolic diff): in HAMILTON_PROMPT.md as a one-iteration upgrade after phase (a) — NOT in the implementation plan (it's a research loop activity, not initial infrastructure)
- Ralph loop prompt: ✓ Task 6
- Cabal target: ✓ Task 1
- All 3 data generators: ✓ Tasks 3, 4, 5

**Placeholder scan:** No TBDs. All code blocks are complete and runnable.

**Type consistency:**
- `HarmonicInput` fields (`hQ`, `hP`, `hDotQ`, `hDotP`) used consistently in Task 3.
- `RigidBodyInput` fields (`rbL`, `rbDotL`) used consistently in Task 4.
- `TwoBodyInput` fields (`tbQ1`…`tbDotP2`) used consistently in Task 5.
- `partialH` and `gradHV3` defined in Task 2, used in Tasks 3/4/5.
- `safeRecip`/`safeSqrt` defined in Task 2, referenced in Tasks 3/5.
