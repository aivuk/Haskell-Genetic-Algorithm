{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Typeable          (Typeable, eqT)
import Data.Type.Equality     ((:~:)(Refl))
import Data.Proxy             (Proxy(..))
import Data.Maybe             (mapMaybe)
import System.Random.Mersenne (MTGen, random, getStdGen)
import Data.Array             (listArray, Array, bounds, (!))
import Data.IORef
import qualified Data.Vector  as V
import Text.Printf
import Control.Monad          (forM, forM_, when, replicateM)
import Data.List              (sortBy, intercalate, minimumBy)
import Data.Ord               (comparing)
import System.IO.Unsafe       (unsafePerformIO)
import Control.Concurrent.Async (mapConcurrently)
import Data.Time.Clock        (getCurrentTime, diffUTCTime, NominalDiffTime)
import System.IO              (hSetBuffering, stdout, BufferMode(..))

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
-- Polymorphic expression tree
------------------------------------------------------------------------

data PTree a b where
    PBin   :: (Typeable x, Typeable y)
           => String -> (x -> y -> b) -> PTree a x -> PTree a y -> PTree a b
    PUn    :: Typeable x
           => String -> (x -> b) -> PTree a x -> PTree a b
    PV     :: String -> (a -> b) -> PTree a b
    PConst :: IORef Double -> PTree a Double
    PConstV :: Vec3 -> PTree a Vec3
    PConstQ :: Quat -> PTree a Quat

instance Show (PTree a b) where
    show (PBin n _ l r) = "(" ++ show l ++ " " ++ n ++ " " ++ show r ++ ")"
    show (PUn  n _ u)   = n ++ "(" ++ show u ++ ")"
    show (PV   n _)     = n
    show (PConst ref)   = show (unsafePerformIO (readIORef ref))
    show (PConstV v)    = showVec v
    show (PConstQ q)    = showQuat q

showVec :: Vec3 -> String
showVec (V3 x y z) = "V3(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

showQuat :: Quat -> String
showQuat (Quaternion w (V3 x y z)) =
    "Q(" ++ show w ++ "," ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

evalTree :: PTree a b -> a -> IO b
evalTree (PBin _ f l r) x = f <$> evalTree l x <*> evalTree r x
evalTree (PUn  _ f u)   x = f <$> evalTree u x
evalTree (PV   _ f)     x = return (f x)
evalTree (PConst ref)   _ = readIORef ref
evalTree (PConstV v)    _ = return v
evalTree (PConstQ q)    _ = return q

sizeP :: PTree a b -> Int
sizeP (PBin _ _ l r) = 1 + sizeP l + sizeP r
sizeP (PUn  _ _ u)   = 1 + sizeP u
sizeP (PV   _ _)     = 1
sizeP (PConst _)     = 1
sizeP (PConstV _)    = 1
sizeP (PConstQ _)    = 1

collectConsts :: PTree a b -> [IORef Double]
collectConsts (PBin _ _ l r) = collectConsts l ++ collectConsts r
collectConsts (PUn  _ _ u)   = collectConsts u
collectConsts (PV   _ _)     = []
collectConsts (PConst ref)   = [ref]
collectConsts (PConstV _)    = []
collectConsts (PConstQ _)    = []

-- True if the tree uses at least one PV leaf (depends on input).
hasLeaf :: PTree a b -> Bool
hasLeaf (PBin _ _ l r) = hasLeaf l || hasLeaf r
hasLeaf (PUn  _ _ u)   = hasLeaf u
hasLeaf (PV   _ _)     = True
hasLeaf _              = False   -- PConst, PConstV, PConstQ

-- Replace constant subtrees (no PV nodes) with a single evaluated constant.
-- This collapses dead branches like (13.7 scale_v (omega add_v neg_v(omega))).
foldConstants :: forall a b. Typeable b
              => PTree a b -> IO (PTree a b)
foldConstants t
    | not (hasLeaf t) = do
        -- Safe: no PV nodes, so any dummy input works
        let dummy = error "foldConstants: evaluated constant subtree with PV — impossible"
        v <- evalTree t dummy
        case eqT @b @Double of
            Just Refl -> do ref <- newIORef v; return (PConst ref)
            Nothing   -> case eqT @b @Vec3 of
                Just Refl -> return (PConstV v)
                Nothing   -> case eqT @b @Quat of
                    Just Refl -> return (PConstQ v)
                    Nothing   -> return t
    | otherwise = case t of
        PBin n f l r -> PBin n f <$> foldConstants l <*> foldConstants r
        PUn  n f u   -> PUn  n f <$> foldConstants u
        _            -> return t

-- | Algebraic normalization: fold neg directly into PConst values.
-- Replaces neg(c) with the literal negated constant node, reducing visual noise.
-- Applied recursively until no more changes occur.
simplifyTree :: forall a b. (Typeable b) => PTree a b -> IO (PTree a b)
simplifyTree t = do
    t' <- step t
    if show t' == show t then return t' else simplifyTree t'
  where
    step :: forall c. Typeable c => PTree a c -> IO (PTree a c)
    step (PBin n f l r) = PBin n f <$> step l <*> step r
    step (PUn n f u)    = do
        u' <- step u
        case eqT @c @Double of
          Just Refl | n == "neg" -> case u' of
            PConst ref -> do v <- readIORef ref; ref' <- newIORef (negate v); return (PConst ref')
            _          -> return (PUn n f u')
          _ -> return (PUn n f u')
    step other = return other

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
        -- Fallbacks: when forced node type has no candidates, try others
        (_,    Just al, _,       _      ) -> pickLeaf al
        (_,    _,       Just au, _      ) -> pickUn   au
        (_,    _,       _,       Just ab) -> pickBin  ab
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
    ]

mocapUns :: [TUn]
mocapUns =
    [ TUn "exp_q" (expQ   :: Vec3   -> Quat)
    , TUn "neg_v" (negate :: Vec3   -> Vec3)
    , TUn "neg"   (negate :: Double -> Double)
    ]

mocapLeaves :: [TLeaf MocapInput]
mocapLeaves =
    [ TLeaf "omega" (miOmega :: MocapInput -> Vec3)
    , TLeaf "dt"    (miDt    :: MocapInput -> Double)
    ]

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

------------------------------------------------------------------------
-- SearchConfig
------------------------------------------------------------------------

data SearchConfig = SearchConfig
    { scMaxWallSeconds  :: Maybe Int
    , scTargetEnergy    :: Maybe Double
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
    { scMaxWallSeconds  = Just 3600
    , scTargetEnergy    = Just 5e-5
    , scMaxRounds       = 5000
    , scStepsPerSwap    = 200
    , scLogEvery        = 10
    , scCheckpointFile  = Just "checkpoint.csv"
    , scCheckpointEvery = 10
    , scTemps           = logTemps 12 0.001 10.0
    , scDepth           = 3
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
parallelTempering bins uns leaves energy cfg = do
    let temps = scTemps cfg
        n     = length temps

    chainGs <- replicateM n getStdGen

    chains <- forM (zip temps chainGs) $ \(_, g') -> do
        t <- genPTree @a @b bins uns leaves (scDepth cfg) g'
        e <- energy t
        newIORef (e, t)

    startTime <- getCurrentTime

    case scCheckpointFile cfg of
        Just fp -> writeFile fp "round,energy,tree\n"
        Nothing -> return ()

    let loop round = do
          now <- getCurrentTime
          let elapsed = diffUTCTime now startTime

          vals <- mapM readIORef chains
          let (bestE, bestT) = minimumBy (comparing fst) vals

          -- Checkpoint and log BEFORE stopping check so we always record
          -- the state that triggered the stop.
          when (round `mod` scLogEvery cfg == 0) $
              printf "[round %d/%d | %s elapsed] best=%.3e\n"
                  round (scMaxRounds cfg) (formatDiff elapsed) bestE

          when (round `mod` scCheckpointEvery cfg == 0) $
              case scCheckpointFile cfg of
                  Just fp -> appendFile fp $
                      show round ++ "," ++ show bestE ++
                      ",\"" ++ show bestT ++ "\"\n"
                  Nothing -> return ()

          let timeLimitHit = case scMaxWallSeconds cfg of
                Just s  -> elapsed >= fromIntegral s
                Nothing -> False

          let energyTargetHit = case scTargetEnergy cfg of
                Just tgt -> bestE <= tgt
                Nothing  -> False

          if | round > scMaxRounds cfg ->
                 return (bestE, bestT, "max rounds reached")
             | timeLimitHit ->
                 return (bestE, bestT, "wall time limit reached")
             | energyTargetHit ->
                 return (bestE, bestT, "target energy reached")
             | otherwise -> do

                 mapConcurrently
                     (\(temp, ref, g') ->
                         forM_ [1 .. scStepsPerSwap cfg :: Int] $ \_ ->
                             mcStep bins uns leaves energy (scDepth cfg) temp ref g')
                     (zip3 temps chains chainGs)

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

                 loop (round + 1)

    loop 1

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    let csvPath = "data/mocap_sample.csv"
    putStrLn $ "Loading " ++ csvPath ++ " ..."
    allPts <- loadMocapCSV csvPath

    -- Subsample for search speed: 300 points captures the structure,
    -- use all points only for final evaluation.
    let pts    = V.take 300 allPts
        allPts' = allPts
    printf "Loaded %d rows, using %d for search\n" (V.length allPts) (V.length pts)

    -- Raw energy used during search (no constant optimisation per step)
    -- Constants are only optimised once on the final best tree at the end.
    let ef  tree = energyMocap pts tree
        cfg = defaultSearchConfig

    printf "Starting search: %d chains, max %d rounds, depth %d\n"
        (length (scTemps cfg)) (scMaxRounds cfg) (scDepth cfg)
    printf "Stopping when: energy < %s, wall time < %s s\n"
        (show (scTargetEnergy cfg)) (show (scMaxWallSeconds cfg))

    (e, best, reason) <- parallelTempering @MocapInput @Quat
                             mocapBins mocapUns mocapLeaves ef cfg

    -- Fine-tune constants on full dataset now that search is done
    optimizeConsts (energyMocap allPts') best
    eFinal <- energyMocap allPts' best
    simplified  <- foldConstants best
    normalized  <- simplifyTree simplified

    putStrLn $ "\nStopped: " ++ reason
    printf "Search energy (300 pts):  %.8f\n" e
    printf "Final energy (all %d pts): %.8f\n" (V.length allPts') eFinal
    printf "Best tree (raw):        %s\n" (show best)
    printf "Best tree (normalized): %s\n" (show normalized)

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
