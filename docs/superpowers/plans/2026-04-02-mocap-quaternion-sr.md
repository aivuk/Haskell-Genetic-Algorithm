# Mocap Quaternion Symbolic Regression Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the multi-typed PTree SR framework to discover `exp_q(ω·dt/2)` from CMU motion capture data, validating the typed approach on a real quaternion problem.

**Architecture:** A new `main-mocap.hs` executable reuses the existing `PTree`/`parallelTempering`/`optimizeConsts` machinery unchanged, adding `Vec3`/`Quat`/`MocapInput` types and a minimal typed function pool. A Python script preprocesses CMU BVH files into a CSV of `(ω, dt, Δq)` tuples. The search adds `SearchConfig` with wall-time limits, progress logging, and checkpointing so long runs can be monitored and interrupted safely.

**Tech Stack:** GHC 9.4.7, `linear` (Vec3/Quaternion), `async` (parallel chains), `time` (wall-clock), Python 3 + numpy + scipy (preprocessing)

---

## File Map

| File | Action | Responsibility |
|------|--------|---------------|
| `genetic-algorithm.cabal` | Modify | Add `linear`, `async`, `time` deps; add `genetic-algorithm-mocap` executable |
| `main-mocap.hs` | Create | All Haskell for the mocap experiment |
| `scripts/preprocess_mocap.py` | Create | BVH → CSV preprocessing |
| `data/mocap_sample.csv` | Generated | Not committed; produced by script |

---

## Task 1: Update cabal file

**Files:**
- Modify: `genetic-algorithm.cabal`

- [ ] **Step 1: Add new executable stanza and dependencies**

Replace the contents of `genetic-algorithm.cabal` with:

```cabal
cabal-version:      3.0
name:               genetic-algorithm
version:            0.1.0.0

executable genetic-algorithm
    main-is:          main.hs
    other-modules:    GeneticProgramming
    build-depends:    base ^>=4.17
                    , mersenne-random
                    , vector
                    , mtl
                    , array
    hs-source-dirs:   .
    default-language: Haskell2010

executable genetic-algorithm-poly
    main-is:          main-poly.hs
    build-depends:    base ^>=4.17
                    , mersenne-random
                    , vector
                    , array
    hs-source-dirs:   .
    default-language: Haskell2010
    ghc-options:      -XTypeApplications

executable genetic-algorithm-mocap
    main-is:          main-mocap.hs
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

- [ ] **Step 2: Verify cabal resolves deps**

```bash
cd /home/aivuk/Haskell-Genetic-Algorithm
cabal build genetic-algorithm-mocap --dry-run 2>&1 | head -30
```

Expected: lists `linear`, `async`, `time` in the build plan. If any are missing from the package index, run `cabal update` first.

- [ ] **Step 3: Commit**

```bash
git add genetic-algorithm.cabal
git commit -m "cabal: add genetic-algorithm-mocap executable with linear/async/time deps"
```

---

## Task 2: Python preprocessing script

**Files:**
- Create: `scripts/preprocess_mocap.py`

- [ ] **Step 1: Write the script**

Create `scripts/preprocess_mocap.py`:

```python
#!/usr/bin/env python3
"""
Preprocess a CMU BVH motion capture file into a CSV suitable for
quaternion symbolic regression.

Output columns: omega_x, omega_y, omega_z, dt, dq_w, dq_x, dq_y, dq_z

Usage:
    python preprocess_mocap.py input.bvh output.csv
"""

import sys
import numpy as np
from scipy.spatial.transform import Rotation


def parse_bvh(path):
    """Return (frame_time, euler_angles_deg) for the root joint.

    Assumes CMU-style BVH where root channels are:
        Xposition Yposition Zposition Zrotation Xrotation Yrotation
    Returns euler angles in (Z, X, Y) order, degrees.
    """
    with open(path) as f:
        lines = f.readlines()

    # Find frame time and motion data start
    frame_time = None
    motion_start = None
    for i, line in enumerate(lines):
        s = line.strip()
        if s.startswith("Frame Time:"):
            frame_time = float(s.split(":")[1].strip())
        if s == "MOTION":
            motion_start = i
    assert frame_time is not None, "Could not find 'Frame Time' in BVH"
    assert motion_start is not None, "Could not find 'MOTION' in BVH"

    # Count root channels to find Euler angle column indices
    # CMU root: Xpos Ypos Zpos Zrot Xrot Yrot → indices 3,4,5
    rot_cols = (3, 4, 5)   # Zrot, Xrot, Yrot

    # Parse frame data (skip "MOTION", "Frames: N", "Frame Time: ...")
    data_lines = lines[motion_start + 3:]
    frames = []
    for line in data_lines:
        line = line.strip()
        if not line:
            continue
        vals = list(map(float, line.split()))
        zr, xr, yr = vals[rot_cols[0]], vals[rot_cols[1]], vals[rot_cols[2]]
        frames.append([zr, xr, yr])

    return frame_time, np.array(frames)   # (N, 3) in ZXY degrees


def euler_to_quats(euler_zxy_deg):
    """Convert (N, 3) ZXY Euler angles in degrees to (N, 4) quaternions [w, x, y, z]."""
    # scipy uses [x,y,z,w]; we reorder to [w,x,y,z] for linear/Haskell convention
    r = Rotation.from_euler('ZXY', euler_zxy_deg, degrees=True)
    q_xyzw = r.as_quat()                 # (N, 4) as [x, y, z, w]
    q_wxyz = q_xyzw[:, [3, 0, 1, 2]]    # reorder to [w, x, y, z]
    return q_wxyz


def ensure_continuity(quats):
    """Flip sign of q[i] if it is on the opposite hemisphere from q[i-1]."""
    out = quats.copy()
    for i in range(1, len(out)):
        if np.dot(out[i], out[i - 1]) < 0:
            out[i] = -out[i]
    return out


def quat_conjugate(q):
    """Conjugate of [w, x, y, z] quaternion."""
    return np.array([q[0], -q[1], -q[2], -q[3]])


def quat_mul(q1, q2):
    """Hamilton product of two [w, x, y, z] quaternions."""
    w1, x1, y1, z1 = q1
    w2, x2, y2, z2 = q2
    return np.array([
        w1*w2 - x1*x2 - y1*y2 - z1*z2,
        w1*x2 + x1*w2 + y1*z2 - z1*y2,
        w1*y2 - x1*z2 + y1*w2 + z1*x2,
        w1*z2 + x1*y2 - y1*x2 + z1*w2,
    ])


def quat_log(q):
    """Quaternion logarithm of a unit quaternion [w, x, y, z].
    Returns pure quaternion [0, vx, vy, vz].
    """
    w = np.clip(q[0], -1.0, 1.0)
    v = q[1:]
    v_norm = np.linalg.norm(v)
    if v_norm < 1e-10:
        return np.zeros(3)
    theta = np.arccos(w)
    return (theta / v_norm) * v


def quat_exp(v):
    """Quaternion exponential of a pure vector v -> unit quaternion [w,x,y,z]."""
    h = np.linalg.norm(v)
    if h < 1e-10:
        return np.array([1.0, 0.0, 0.0, 0.0])
    return np.array([np.cos(h), *(np.sin(h) / h * v)])


def compute_rows(quats, dt):
    """Compute (omega, dt, delta_q) rows from a quaternion sequence."""
    rows = []
    for i in range(1, len(quats)):
        q_prev = quats[i - 1]
        q_curr = quats[i]
        dq = quat_mul(quat_conjugate(q_prev), q_curr)
        # Normalise dq to correct float drift
        dq = dq / np.linalg.norm(dq)
        # omega = 2 * log(dq) / dt
        log_dq = quat_log(dq)
        omega = 2.0 * log_dq / dt
        rows.append((omega[0], omega[1], omega[2], dt,
                     dq[0], dq[1], dq[2], dq[3]))
    return rows


def validate(rows):
    """Reconstruct dq from omega via exp_q(omega*dt/2) and check round-trip error."""
    errors = []
    for (ox, oy, oz, dt, dw, dx, dy, dz) in rows:
        omega = np.array([ox, oy, oz])
        dq_target = np.array([dw, dx, dy, dz])
        dq_hat = quat_exp(omega * dt / 2.0)
        # Handle double cover
        d = abs(np.dot(dq_hat, dq_target))
        errors.append(1.0 - d * d)
    mean_err = float(np.mean(errors))
    print(f"Validation: mean quaternion loss = {mean_err:.2e}  (target < 1e-6)")
    assert mean_err < 1e-6, f"Round-trip validation FAILED: mean loss = {mean_err}"


def main():
    if len(sys.argv) != 3:
        print("Usage: python preprocess_mocap.py input.bvh output.csv")
        sys.exit(1)

    bvh_path, csv_path = sys.argv[1], sys.argv[2]

    print(f"Parsing {bvh_path} ...")
    dt, euler = parse_bvh(bvh_path)
    print(f"  {len(euler)} frames, frame_time={dt:.6f}s")

    quats = euler_to_quats(euler)
    quats = ensure_continuity(quats)

    rows = compute_rows(quats, dt)
    print(f"  {len(rows)} transition rows")

    validate(rows)

    with open(csv_path, "w") as f:
        f.write("omega_x,omega_y,omega_z,dt,dq_w,dq_x,dq_y,dq_z\n")
        for row in rows:
            f.write(",".join(f"{v:.10f}" for v in row) + "\n")

    print(f"Written to {csv_path}")


if __name__ == "__main__":
    main()
```

- [ ] **Step 2: Verify script syntax**

```bash
python3 -c "import scripts.preprocess_mocap" 2>/dev/null || python3 scripts/preprocess_mocap.py 2>&1 | head -5
```

Expected: `Usage: python preprocess_mocap.py input.bvh output.csv`

- [ ] **Step 3: Commit**

```bash
git add scripts/preprocess_mocap.py
git commit -m "scripts: add BVH to CSV preprocessing for mocap SR experiment"
```

---

## Task 3: Download CMU data and generate CSV

**Files:**
- Generate: `data/mocap_sample.csv`

- [ ] **Step 1: Install Python dependencies**

```bash
pip install numpy scipy
```

Expected: both packages install (or already satisfied).

- [ ] **Step 2: Download a CMU BVH file**

CMU subject 09, motion 01 (walking) is a well-studied clip. Download it:

```bash
mkdir -p data
# CMU BVH mirror (cgspeed conversions, standard Euler convention)
wget -O data/09_01.bvh "https://raw.githubusercontent.com/una-dinosauria/cmu-mocap/master/bvh/09/09_01.bvh" 2>/dev/null \
  || curl -o data/09_01.bvh "https://raw.githubusercontent.com/una-dinosauria/cmu-mocap/master/bvh/09/09_01.bvh"
```

If that mirror is unavailable, download any walking BVH from http://mocap.cs.cmu.edu/ (subject 09) and save to `data/09_01.bvh`. Alternatively use any BVH file with the standard CMU root channel layout (Xpos Ypos Zpos Zrot Xrot Yrot).

- [ ] **Step 3: Run preprocessing**

```bash
python3 scripts/preprocess_mocap.py data/09_01.bvh data/mocap_sample.csv
```

Expected output:
```
Parsing data/09_01.bvh ...
  NNNN frames, frame_time=0.008333s
  NNNN transition rows
Validation: mean quaternion loss = X.XXe-XX  (target < 1e-6)
Written to data/mocap_sample.csv
```

The validation line must show loss < 1e-6. If it fails, the BVH file uses a different Euler convention — inspect `data/09_01.bvh` HIERARCHY section for root channel order and adjust `rot_cols` and the `from_euler` axis string in `parse_bvh`.

- [ ] **Step 4: Inspect CSV**

```bash
head -3 data/mocap_sample.csv
wc -l data/mocap_sample.csv
```

Expected: header row + numeric data, at least 1000 rows.

- [ ] **Step 5: Commit (script only, not CSV)**

```bash
echo "data/mocap_sample.csv" >> .gitignore
echo "data/*.bvh" >> .gitignore
git add .gitignore
git commit -m "gitignore: exclude generated mocap data files"
```

---

## Task 4: Scaffold main-mocap.hs — types and imports

**Files:**
- Create: `main-mocap.hs`

- [ ] **Step 1: Write the scaffold**

Create `main-mocap.hs`:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Data.Typeable          (Typeable, eqT)
import Data.Type.Equality     ((:~:)(Refl))
import Data.Proxy             (Proxy(..))
import Data.Maybe             (mapMaybe)
import System.Random.Mersenne (MTGen, random, getStdGen)
import Data.Array             (listArray, Array, bounds, (!))
import Data.IORef
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector         as V
import Text.Printf
import Control.Monad          (forM, forM_, when, replicateM)
import Data.List              (sortBy, intercalate)
import Data.Ord               (comparing)
import System.IO.Unsafe       (unsafePerformIO)
import Control.Concurrent.Async (mapConcurrently)
import Data.Time.Clock        (getCurrentTime, diffUTCTime, NominalDiffTime)

-- linear
import Linear.V3              (V3(..))
import Linear.Quaternion      (Quaternion(..))
import Linear.Metric          (dot, norm)
import Linear.Vector          ((*^))

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

type Vec3 = V3 Double
type Quat = Quaternion Double

data MocapInput = MocapInput
    { miOmega :: Vec3
    , miDt    :: Double
    } deriving (Show)

------------------------------------------------------------------------
-- Polymorphic expression tree (copied verbatim from main-poly.hs)
------------------------------------------------------------------------

data PTree a b where
    PBin   :: (Typeable x, Typeable y)
           => String -> (x -> y -> b) -> PTree a x -> PTree a y -> PTree a b
    PUn    :: Typeable x
           => String -> (x -> b) -> PTree a x -> PTree a b
    PV     :: String -> (a -> b) -> PTree a b
    PConst :: IORef Double -> PTree a Double

instance Show (PTree a b) where
    show (PBin n _ l r) = "(" ++ show l ++ " " ++ n ++ " " ++ show r ++ ")"
    show (PUn  n _ u)   = n ++ "(" ++ show u ++ ")"
    show (PV   n _)     = n
    show (PConst ref)   = show (unsafePerformIO (readIORef ref))

evalTree :: PTree a b -> a -> IO b
evalTree (PBin _ f l r) x = f <$> evalTree l x <*> evalTree r x
evalTree (PUn  _ f u)   x = f <$> evalTree u x
evalTree (PV   _ f)     x = return (f x)
evalTree (PConst ref)   _ = readIORef ref

sizeP :: PTree a b -> Int
sizeP (PBin _ _ l r) = 1 + sizeP l + sizeP r
sizeP (PUn  _ _ u)   = 1 + sizeP u
sizeP (PV   _ _)     = 1
sizeP (PConst _)     = 1

collectConsts :: PTree a b -> [IORef Double]
collectConsts (PBin _ _ l r) = collectConsts l ++ collectConsts r
collectConsts (PUn  _ _ u)   = collectConsts u
collectConsts (PV   _ _)     = []
collectConsts (PConst ref)   = [ref]

------------------------------------------------------------------------
-- Open function pool types
------------------------------------------------------------------------

data TBin = forall x y z. (Typeable x, Typeable y, Typeable z) =>
    TBin String (x -> y -> z)

data TUn = forall x y. (Typeable x, Typeable y) =>
    TUn String (x -> y)

data TLeaf a = forall y. Typeable y =>
    TLeaf String (a -> y)

------------------------------------------------------------------------
-- Runtime type matching
------------------------------------------------------------------------

data MatchedBin b = forall x y. (Typeable x, Typeable y) =>
    MatchedBin String (Proxy x) (Proxy y) (x -> y -> b)

data MatchedUn b = forall x. Typeable x =>
    MatchedUn String (Proxy x) (x -> b)

matchBin :: forall b. Typeable b => TBin -> Maybe (MatchedBin b)
matchBin (TBin name (f :: x -> y -> z)) =
    case eqT @b @z of
        Just Refl -> Just (MatchedBin name (Proxy @x) (Proxy @y) f)
        Nothing   -> Nothing

matchUn :: forall b. Typeable b => TUn -> Maybe (MatchedUn b)
matchUn (TUn name (f :: x -> y)) =
    case eqT @b @y of
        Just Refl -> Just (MatchedUn name (Proxy @x) f)
        Nothing   -> Nothing

matchLeaf :: forall a b. Typeable b => TLeaf a -> Maybe (String, a -> b)
matchLeaf (TLeaf name (f :: a -> y)) =
    case eqT @b @y of
        Just Refl -> Just (name, f)
        Nothing   -> Nothing

------------------------------------------------------------------------
-- Random helpers
------------------------------------------------------------------------

toArr :: [a] -> Maybe (Array Int a)
toArr [] = Nothing
toArr l  = Just $ listArray (0, length l - 1) l

randElem :: Array Int a -> MTGen -> IO a
randElem arr g = do
    i <- fmap (`mod` (snd (bounds arr) + 1)) (random g :: IO Int)
    return (arr ! i)

data NodeType = BinT | UnT | VT

randNodeType :: MTGen -> IO NodeType
randNodeType g = do
    i <- fmap (`mod` 3) (random g :: IO Int)
    return $ [BinT, UnT, VT] !! i

main :: IO ()
main = putStrLn "main-mocap: scaffold ok"
```

- [ ] **Step 2: Build to verify types compile**

```bash
cd /home/aivuk/Haskell-Genetic-Algorithm
cabal build genetic-algorithm-mocap 2>&1
```

Expected: build succeeds, no type errors. If `linear` types are not `Typeable`, GHC will report a missing instance — in that case add `import Data.Typeable (Typeable)` and check that `linear >= 1.21` is resolved (all modern versions auto-derive Typeable).

- [ ] **Step 3: Commit**

```bash
git add main-mocap.hs
git commit -m "mocap: scaffold with PTree machinery, Vec3/Quat types, MocapInput"
```

---

## Task 5: Implement function pool and tree generation

**Files:**
- Modify: `main-mocap.hs`

- [ ] **Step 1: Add genPTree and mutatePTree (after the random helpers section)**

Append to `main-mocap.hs` before the `main` function:

```haskell
------------------------------------------------------------------------
-- Tree generation
------------------------------------------------------------------------

genPTree :: forall a b. (Typeable a, Typeable b)
         => [TBin] -> [TUn] -> [TLeaf a]
         -> Int -> MTGen -> IO (PTree a b)
genPTree bins uns leaves depth g = do
    let vl = mapMaybe (matchLeaf @a @b) leaves
        vu = mapMaybe (matchUn   @b)    uns
        vb = mapMaybe (matchBin  @b)    bins
    nt <- if depth <= 0 then return VT else randNodeType g
    case (nt, toArr vl, toArr vu, toArr vb) of
        (VT,   Just al, _,       _      ) -> pickLeaf al
        (UnT,  _,       Just au, _      ) -> pickUn   au
        (BinT, _,       _,       Just ab) -> pickBin  ab
        (_,    Just al, _,       _      ) -> pickLeaf al
        _ -> error "genPTree: no leaf for this output type"
  where
    maybeConst :: IO (Maybe (PTree a b))
    maybeConst = case eqT @b @Double of
        Just Refl -> do
            p <- (random g :: IO Double)
            if p < 0.25
                then do v   <- fmap (\r -> r * 4 - 2) (random g :: IO Double)
                        ref <- newIORef v
                        return (Just (PConst ref))
                else return Nothing
        Nothing -> return Nothing

    pickLeaf al = do
        mc <- maybeConst
        case mc of
            Just c  -> return c
            Nothing -> do (name, f) <- randElem al g
                          return (PV name f)

    pickUn au = do
        MatchedUn name (Proxy :: Proxy x) f <- randElem au g
        sub <- genPTree @a @x bins uns leaves (depth - 1) g
        return (PUn name f sub)

    pickBin ab = do
        MatchedBin name (Proxy :: Proxy x) (Proxy :: Proxy y) f <- randElem ab g
        l <- genPTree @a @x bins uns leaves (depth - 1) g
        r <- genPTree @a @y bins uns leaves (depth - 1) g
        return (PBin name f l r)

mutatePTree :: forall a b. (Typeable a, Typeable b)
            => PTree a b
            -> [TBin] -> [TUn] -> [TLeaf a]
            -> Int -> MTGen -> IO (PTree a b)
mutatePTree tree bins uns leaves maxDepth g = do
    node <- fmap (`mod` sizeP tree) (random g :: IO Int)
    go tree node
  where
    go :: forall c. Typeable c => PTree a c -> Int -> IO (PTree a c)
    go _ 0 = genPTree @a @c bins uns leaves maxDepth g
    go t@(PV _ _)    _ = return t
    go t@(PConst _)  _ = return t
    go (PUn name f sub) n =
        PUn name f <$> go sub (n - 1)
    go (PBin name f l r) n =
        let sl = sizeP l
        in if n - 1 < sl
           then (\l' -> PBin name f l' r) <$> go l (n - 1)
           else (\r' -> PBin name f l r') <$> go r (n - 1 - sl)

------------------------------------------------------------------------
-- Quaternion helper
------------------------------------------------------------------------

-- Quaternion exponential of a pure vector: exp_q(v) = cos(||v||) + sin(||v||)/||v|| * v
-- Handles ||v|| -> 0 gracefully (returns identity).
expQ :: Vec3 -> Quat
expQ v =
    let h = norm v
    in if h < 1e-10
       then Quaternion 1 (V3 0 0 0)
       else Quaternion (cos h) ((sin h / h) *^ v)

------------------------------------------------------------------------
-- Mocap function pool
------------------------------------------------------------------------

mocapBins :: [TBin]
mocapBins =
    [ TBin "(*)"     ((*) :: Double -> Double -> Double)
    , TBin "scale_v" ((*^) :: Double -> Vec3 -> Vec3)
    , TBin "add_v"   ((+)  :: Vec3 -> Vec3 -> Vec3)
    , TBin "q_mul"   ((*) :: Quat -> Quat -> Quat)
    ]

mocapUns :: [TUn]
mocapUns =
    [ TUn "exp_q"  (expQ   :: Vec3   -> Quat)
    , TUn "neg_v"  (negate :: Vec3   -> Vec3)
    , TUn "norm_v" (norm   :: Vec3   -> Double)
    , TUn "neg"    (negate :: Double -> Double)
    ]

mocapLeaves :: [TLeaf MocapInput]
mocapLeaves =
    [ TLeaf "omega" (miOmega :: MocapInput -> Vec3)
    , TLeaf "dt"    (miDt    :: MocapInput -> Double)
    ]
```

- [ ] **Step 2: Build to verify**

```bash
cabal build genetic-algorithm-mocap 2>&1
```

Expected: clean build. If `norm` is ambiguous (Linear vs Prelude), add `import Prelude hiding (())` is not needed — but if there's a clash, qualify as `Linear.Metric.norm`.

- [ ] **Step 3: Commit**

```bash
git add main-mocap.hs
git commit -m "mocap: add genPTree, mutatePTree, expQ, and typed function pool"
```

---

## Task 6: CSV loader and energy function

**Files:**
- Modify: `main-mocap.hs`

- [ ] **Step 1: Add CSV loader and energy function**

Append before the `main` function:

```haskell
------------------------------------------------------------------------
-- CSV loader
------------------------------------------------------------------------

splitOn :: Char -> String -> [String]
splitOn delim = foldr f [""]
  where
    f c (x:xs) | c == delim = "" : x : xs
               | otherwise  = (c:x) : xs
    f _ [] = []

-- Load mocap CSV with columns:
-- omega_x, omega_y, omega_z, dt, dq_w, dq_x, dq_y, dq_z
loadMocapCSV :: FilePath -> IO (V.Vector (Vec3, Double, Quat))
loadMocapCSV path = do
    content <- readFile path
    let ls = drop 1 (lines content)   -- skip header
        parseLine l =
            let cols = splitOn ',' l
            in case map (read :: String -> Double) cols of
                [ox, oy, oz, dt, dw, dx, dy, dz] ->
                    Just (V3 ox oy oz, dt, Quaternion dw (V3 dx dy dz))
                _ -> Nothing
    return $ V.fromList $ mapMaybe parseLine ls

------------------------------------------------------------------------
-- Constant optimisation (golden section, coordinate descent)
------------------------------------------------------------------------

goldenSearchIO :: (Double -> IO Double) -> Double -> Double -> Int -> IO Double
goldenSearchIO f lo hi n = go lo hi n
  where
    phi = (sqrt 5 - 1) / 2
    go a b 0 = return ((a + b) / 2)
    go a b k = do
        let c = b - phi * (b - a)
            d = a + phi * (b - a)
        fc <- f c
        fd <- f d
        if fc < fd then go a d (k - 1)
                   else go c b (k - 1)

optimizeConsts :: (PTree MocapInput Quat -> IO Double)
               -> PTree MocapInput Quat -> IO ()
optimizeConsts energy tree = do
    let refs = collectConsts tree
    forM_ [1 .. 3 :: Int] $ \_ ->
        forM_ refs $ \ref -> do
            v0 <- readIORef ref
            let radius = max 1.0 (abs v0 * 2)
                obj v  = writeIORef ref v >> energy tree
            vOpt <- goldenSearchIO obj (v0 - radius) (v0 + radius) 40
            writeIORef ref vOpt

------------------------------------------------------------------------
-- Energy function
------------------------------------------------------------------------

energyMocap :: V.Vector (Vec3, Double, Quat) -> PTree MocapInput Quat -> IO Double
energyMocap pts tree = do
    errs <- forM (V.toList pts) $ \(w, d, q_target) -> do
        q_hat <- evalTree tree (MocapInput w d)
        let d_val = q_hat `dot` q_target
        return (1.0 - d_val * d_val)
    return $ sum errs / fromIntegral (length errs)
```

- [ ] **Step 2: Build to verify**

```bash
cabal build genetic-algorithm-mocap 2>&1
```

Expected: clean build.

- [ ] **Step 3: Commit**

```bash
git add main-mocap.hs
git commit -m "mocap: add CSV loader, constant optimiser, and dot-product energy function"
```

---

## Task 7: SearchConfig and parallel tempering with stopping/logging/checkpointing

**Files:**
- Modify: `main-mocap.hs`

- [ ] **Step 1: Add SearchConfig and the improved parallelTempering**

Append before the `main` function:

```haskell
------------------------------------------------------------------------
-- SearchConfig
------------------------------------------------------------------------

data SearchConfig = SearchConfig
    { scMaxWallSeconds  :: Maybe Int      -- Nothing = unlimited
    , scTargetEnergy    :: Maybe Double   -- stop when energy < this
    , scMaxRounds       :: Int
    , scStepsPerSwap    :: Int
    , scLogEvery        :: Int
    , scCheckpointFile  :: Maybe FilePath
    , scCheckpointEvery :: Int
    , scTemps           :: [Double]
    , scDepth           :: Int
    }

defaultSearchConfig :: SearchConfig
defaultSearchConfig = SearchConfig
    { scMaxWallSeconds  = Just 3600     -- 1 hour hard cap
    , scTargetEnergy    = Just 1e-4
    , scMaxRounds       = 5000
    , scStepsPerSwap    = 200
    , scLogEvery        = 10
    , scCheckpointFile  = Just "checkpoint.csv"
    , scCheckpointEvery = 50
    , scTemps           = logTemps 12 0.001 10.0
    , scDepth           = 5
    }

logTemps :: Int -> Double -> Double -> [Double]
logTemps n tMin tMax =
    [ tMin * (tMax / tMin) ** (fromIntegral i / fromIntegral (n - 1))
    | i <- [0 .. n - 1] ]

------------------------------------------------------------------------
-- Format elapsed time as HH:MM:SS
------------------------------------------------------------------------

formatDiff :: NominalDiffTime -> String
formatDiff d =
    let total = floor d :: Int
        h     = total `div` 3600
        m     = (total `mod` 3600) `div` 60
        s     = total `mod` 60
    in printf "%02d:%02d:%02d" h m s

------------------------------------------------------------------------
-- MC step (single chain)
------------------------------------------------------------------------

mcStep :: forall a b. (Typeable a, Typeable b)
       => [TBin] -> [TUn] -> [TLeaf a]
       -> (PTree a b -> IO Double)
       -> Int -> Double
       -> IORef (Double, PTree a b)
       -> MTGen -> IO ()
mcStep bins uns leaves energy depth temp ref g = do
    (e, t) <- readIORef ref
    t'     <- mutatePTree t bins uns leaves depth g
    e'     <- energy t'
    p      <- (random g :: IO Double)
    when (p < exp (-(e' - e) / temp)) $
        writeIORef ref (e', t')

------------------------------------------------------------------------
-- Parallel tempering with SearchConfig
------------------------------------------------------------------------

parallelTempering :: forall a b. (Typeable a, Typeable b)
                  => [TBin] -> [TUn] -> [TLeaf a]
                  -> (PTree a b -> IO Double)
                  -> SearchConfig
                  -> IO (Double, PTree a b, String)
                    -- ^ (energy, tree, stop-reason)
parallelTempering bins uns leaves energy cfg = do
    let temps = scTemps cfg
        n     = length temps

    -- One generator per chain
    chainGs <- replicateM n getStdGen

    -- Initialise chains
    chains <- forM (zip temps chainGs) $ \(_, g') -> do
        t <- genPTree @a @b bins uns leaves (scDepth cfg) g'
        e <- energy t
        newIORef (e, t)

    startTime <- getCurrentTime

    -- Write checkpoint header
    case scCheckpointFile cfg of
        Just fp -> writeFile fp "round,energy,tree\n"
        Nothing -> return ()

    let loop round = do
          now <- getCurrentTime
          let elapsed = diffUTCTime now startTime

          -- Check wall-time limit
          let timeLimitHit = case scMaxWallSeconds cfg of
                Just s  -> elapsed >= fromIntegral s
                Nothing -> False

          -- Find current best across all chains
          vals <- mapM readIORef chains
          let (bestE, bestT) = minimumBy (comparing fst) vals

          -- Check energy target
          let energyTargetHit = case scTargetEnergy cfg of
                Just t  -> bestE <= t
                Nothing -> False

          -- Stopping conditions
          if | round > scMaxRounds cfg ->
                 return (bestE, bestT, "max rounds reached")
             | timeLimitHit ->
                 return (bestE, bestT, "wall time limit reached")
             | energyTargetHit ->
                 return (bestE, bestT, "target energy reached")
             | otherwise -> do

                 -- Run each chain's MC steps in parallel
                 mapConcurrently
                     (\(temp, ref, g') ->
                         forM_ [1 .. scStepsPerSwap cfg :: Int] $ \_ ->
                             mcStep bins uns leaves energy (scDepth cfg) temp ref g')
                     (zip3 temps chains chainGs)

                 -- Swap phase (sequential, cheap)
                 forM_ (zip3 temps (tail temps) (zip chains (tail chains))) $
                     \(t1, t2, (ref1, ref2)) -> do
                         (e1, _) <- readIORef ref1
                         (e2, _) <- readIORef ref2
                         g0      <- getStdGen
                         p       <- (random g0 :: IO Double)
                         when (p < exp ((1/t1 - 1/t2) * (e1 - e2))) $ do
                             v1 <- readIORef ref1
                             v2 <- readIORef ref2
                             writeIORef ref1 v2
                             writeIORef ref2 v1

                 -- Progress log
                 when (round `mod` scLogEvery cfg == 0) $ do
                     vals' <- mapM readIORef chains
                     let (be, _) = minimumBy (comparing fst) vals'
                     printf "[round %d/%d | %s elapsed] best=%.6f\n"
                         round (scMaxRounds cfg) (formatDiff elapsed) be

                 -- Checkpoint
                 when (round `mod` scCheckpointEvery cfg == 0) $
                     case scCheckpointFile cfg of
                         Just fp -> do
                             vals' <- mapM readIORef chains
                             let (be, bt) = minimumBy (comparing fst) vals'
                             appendFile fp $
                                 show round ++ "," ++ show be ++
                                 ",\"" ++ show bt ++ "\"\n"
                         Nothing -> return ()

                 loop (round + 1)

    loop 1
```

- [ ] **Step 2: Add required imports and extension at the top of main-mocap.hs**

Add `{-# LANGUAGE MultiWayIf #-}` to the pragma block at the top of the file (needed for `if | ... |` syntax in the loop). Add `minimumBy` to the `Data.List` import:

```haskell
import Data.List (sortBy, intercalate, minimumBy)
```

- [ ] **Step 3: Build to verify**

```bash
cabal build genetic-algorithm-mocap 2>&1
```

Expected: clean build with no type or extension errors.

- [ ] **Step 4: Commit**

```bash
git add main-mocap.hs
git commit -m "mocap: add SearchConfig, parallel tempering with stopping/logging/checkpointing"
```

---

## Task 8: Wire up main and run smoke test

**Files:**
- Modify: `main-mocap.hs`

- [ ] **Step 1: Replace the stub `main` with the real one**

Replace:
```haskell
main :: IO ()
main = putStrLn "main-mocap: scaffold ok"
```

With:

```haskell
main :: IO ()
main = do
    let csvPath = "data/mocap_sample.csv"
    putStrLn $ "Loading " ++ csvPath ++ " ..."
    pts <- loadMocapCSV csvPath
    printf "Loaded %d transition rows\n" (V.length pts)

    let ef tree = optimizeConsts (energyMocap pts) tree >> energyMocap pts tree
        cfg = defaultSearchConfig

    printf "Starting search: %d chains, max %d rounds, depth %d\n"
        (length (scTemps cfg)) (scMaxRounds cfg) (scDepth cfg)
    printf "Stopping when: energy < %s, wall time < %s s\n"
        (show (scTargetEnergy cfg)) (show (scMaxWallSeconds cfg))

    (e, best, reason) <- parallelTempering @MocapInput @Quat
                             mocapBins mocapUns mocapLeaves ef cfg

    putStrLn $ "\nStopped: " ++ reason
    printf "Best energy (1 - |q̂·q|²): %.8f\n" e
    printf "Best tree: %s\n" (show best)

    putStrLn "\nSample predictions (first 5 rows):"
    printf "%-12s %-12s %-12s  dq_target_w  dq_found_w\n"
        ("omega_x"::String) ("omega_y"::String) ("omega_z"::String)
    V.mapM_ (\(w@(V3 ox oy oz), d, q_t) -> do
        q_h <- evalTree best (MocapInput w d)
        let Quaternion tw _ = q_t
            Quaternion hw _ = q_h
        printf "%-12.4f %-12.4f %-12.4f  %-12.6f  %-12.6f\n"
            ox oy oz tw hw
        ) (V.take 5 pts)
```

- [ ] **Step 2: Full build**

```bash
cabal build genetic-algorithm-mocap 2>&1
```

Expected: clean build.

- [ ] **Step 3: Smoke test — 10 rounds**

Edit `defaultSearchConfig` temporarily to use `scMaxRounds = 10` and `scMaxWallSeconds = Just 60`, then run:

```bash
cabal run genetic-algorithm-mocap +RTS -N4 2>&1
```

Expected output (values will vary):
```
Loading data/mocap_sample.csv ...
Loaded NNNN transition rows
Starting search: 12 chains, max 10 rounds, depth 5
Stopping when: energy < Just 1.0e-4, wall time < Just 3600 s
[round 10/10 | 00:00:XX elapsed] best=0.XXXXXX
Stopped: max rounds reached
Best energy (1 - |q̂·q|²): 0.XXXXXXXX
Best tree: ...
Sample predictions (first 5 rows):
...
```

The smoke test passes if: no crashes, energy is printed, tree is printed, sample rows appear. Energy value after 10 rounds will be high (near 1) — that is expected.

- [ ] **Step 4: Restore defaultSearchConfig to full settings**

Revert `scMaxRounds` back to `5000` and `scMaxWallSeconds` back to `Just 3600`.

- [ ] **Step 5: Final build verification**

```bash
cabal build genetic-algorithm-mocap 2>&1
```

- [ ] **Step 6: Commit**

```bash
git add main-mocap.hs
git commit -m "mocap: wire up main, end-to-end smoke test passing"
```

---

## Completion Verification

Run the following to confirm all three completion criteria are met:

```bash
# 1. cabal build succeeds
cabal build genetic-algorithm-mocap 2>&1 | tail -3

# 2. CSV exists and has data
wc -l data/mocap_sample.csv

# 3. Short run produces output without crashing
cabal run genetic-algorithm-mocap +RTS -N4 -- 2>&1 | head -20
```

For the ralph-loop completion promise, output:

```
<promise>IMPLEMENTATION COMPLETE</promise>
```

only when all three checks above pass.
