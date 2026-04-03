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
import qualified Data.Vector.Unboxed as V
import Text.Printf
import Control.Monad          (forM, forM_, when)
import Data.List              (sortBy)
import Data.Ord               (comparing)
import System.IO.Unsafe       (unsafePerformIO)

------------------------------------------------------------------------
-- Polymorphic expression tree
-- PConst carries a mutable IORef Double for constant optimisation.
------------------------------------------------------------------------

data PTree a b where
    PBin   :: (Typeable x, Typeable y)
           => String -> (x -> y -> b) -> PTree a x -> PTree a y -> PTree a b
    PUn    :: Typeable x
           => String -> (x -> b) -> PTree a x -> PTree a b
    PV     :: String -> (a -> b) -> PTree a b
    PConst :: IORef Double -> PTree a Double

-- Show uses unsafePerformIO only for display; the shown value is the
-- current optimised constant, which is safe since Show is never called
-- concurrently with writes.
instance Show (PTree a b) where
    show (PBin n _ l r) = "(" ++ show l ++ " " ++ n ++ " " ++ show r ++ ")"
    show (PUn  n _ u)   = n ++ "(" ++ show u ++ ")"
    show (PV   n _)     = n
    show (PConst ref)   = show (unsafePerformIO (readIORef ref))

-- IO-based evaluation so PConst reads are always fresh
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

-- Collect all mutable constant refs from a tree
collectConsts :: PTree a b -> [IORef Double]
collectConsts (PBin _ _ l r) = collectConsts l ++ collectConsts r
collectConsts (PUn  _ _ u)   = collectConsts u
collectConsts (PV   _ _)     = []
collectConsts (PConst ref)   = [ref]

------------------------------------------------------------------------
-- Open function pool
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
-- Random PTree generation
-- When the output type is Double and depth == 0, create a PConst with
-- 25% probability, giving the optimiser concrete numbers to tune.
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
        _ -> error "genPTree: no leaf for this output type — add a leaf"
  where
    -- 25% chance of a PConst when output type is Double
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

------------------------------------------------------------------------
-- Subtree mutation (PConst is treated as a leaf)
------------------------------------------------------------------------

mutatePTree :: forall a b. (Typeable a, Typeable b)
            => PTree a b
            -> [TBin] -> [TUn] -> [TLeaf a]
            -> Int -> MTGen -> IO (PTree a b)
mutatePTree tree bins uns leaves maxDepth g = do
    node <- fmap (`mod` sizeP tree) (random g :: IO Int)
    go tree node
  where
    go :: forall c. Typeable c => PTree a c -> Int -> IO (PTree a c)
    go _ 0 =
        genPTree @a @c bins uns leaves maxDepth g
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
-- Function pool  (Double and Bool)
------------------------------------------------------------------------

pexp :: Double -> Double
pexp = exp . min 13

plog :: Double -> Double
plog = log . max 1e-17

psqr :: Double -> Double
psqr = sqrt . abs

pdiv :: Double -> Double -> Double
pdiv x y | abs y >= 1e-4 = x / y
          | otherwise    = 1e4 * signum x

b2d :: Bool -> Double
b2d b = if b then 1.0 else 0.0

myBins :: [TBin]
myBins =
    [ TBin "(+)"  ((+)  :: Double -> Double -> Double)
    , TBin "(-)"  ((-)  :: Double -> Double -> Double)
    , TBin "(*)"  ((*) :: Double -> Double -> Double)
    , TBin "(/)"  (pdiv :: Double -> Double -> Double)
    , TBin "(&&)" ((&&) :: Bool -> Bool -> Bool)
    , TBin "(||)" ((||) :: Bool -> Bool -> Bool)
    ]

myUns :: [TUn]
myUns =
    [ TUn "sin"     (sin          :: Double -> Double)
    , TUn "cos"     (cos          :: Double -> Double)
    , TUn "sqr"     (psqr         :: Double -> Double)
    , TUn "exp"     (pexp         :: Double -> Double)
    , TUn "log"     (plog         :: Double -> Double)
    , TUn "neg"     (negate       :: Double -> Double)
    , TUn "sigmoid" ((\x -> 1/(1+exp(-x))) :: Double -> Double)
    , TUn ">0"      ((> 0)        :: Double -> Bool)
    , TUn "b2d"     (b2d          :: Bool   -> Double)
    , TUn "not"     (not          :: Bool   -> Bool)
    ]

myLeaves :: [TLeaf Double]
myLeaves =
    [ TLeaf "x"   (id    :: Double -> Double)
    , TLeaf "x>0" ((> 0) :: Double -> Bool)
    , TLeaf "x<0" ((< 0) :: Double -> Bool)
    ]

------------------------------------------------------------------------
-- Energy  (IO-based to support PConst reads)
------------------------------------------------------------------------

energyF :: V.Vector (Double, Double) -> PTree Double Double -> IO Double
energyF pts tree = do
    errs <- forM (V.toList pts) $ \(x, y) -> do
        y' <- evalTree tree x
        return ((y - y') ^ 2)
    return $ sum errs / fromIntegral (length errs)

------------------------------------------------------------------------
-- Constant optimisation  (coordinate descent + golden section search)
------------------------------------------------------------------------

-- 1-D golden section search to minimise f on [lo, hi]
goldenSearchIO :: (Double -> IO Double) -> Double -> Double -> Int -> IO Double
goldenSearchIO f lo hi n = go lo hi n
  where
    phi = (sqrt 5 - 1) / 2
    go lo hi 0 = return ((lo + hi) / 2)
    go lo hi k = do
        let c = hi - phi * (hi - lo)
            d = lo + phi * (hi - lo)
        fc <- f c
        fd <- f d
        if fc < fd then go lo d (k - 1)
                   else go c hi (k - 1)

-- Optimise every PConst in the tree by coordinate descent.
-- Each constant is searched over [v - radius, v + radius].
optimizeConsts :: V.Vector (Double, Double) -> PTree Double Double -> IO ()
optimizeConsts pts tree = do
    let refs = collectConsts tree
    forM_ [1 .. 3 :: Int] $ \_ ->        -- 3 passes of coordinate descent
        forM_ refs $ \ref -> do
            v0 <- readIORef ref
            let radius = max 1.0 (abs v0 * 2)
                obj v  = writeIORef ref v >> energyF pts tree
            vOpt <- goldenSearchIO obj (v0 - radius) (v0 + radius) 40
            writeIORef ref vOpt

------------------------------------------------------------------------
-- MC step and parallel tempering
-- energy :: PTree a b -> IO Double  (encapsulates both optim + eval)
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

parallelTempering :: forall a b. (Typeable a, Typeable b)
                  => [TBin] -> [TUn] -> [TLeaf a]
                  -> (PTree a b -> IO Double)
                  -> Int -> Int -> Int -> [Double]
                  -> MTGen
                  -> IO (Double, PTree a b)
parallelTempering bins uns leaves energy depth stepsPerSwap nRounds temps g = do
    chains <- forM temps $ \_ -> do
        t  <- genPTree @a @b bins uns leaves depth g
        e  <- energy t
        newIORef (e, t)
    forM_ [1 .. nRounds :: Int] $ \_ -> do
        forM_ (zip temps chains) $ \(temp, ref) ->
            forM_ [1 .. stepsPerSwap :: Int] $ \_ ->
                mcStep bins uns leaves energy depth temp ref g
        forM_ (zip3 temps (tail temps) (zip chains (tail chains))) $
            \(t1, t2, (ref1, ref2)) -> do
                (e1, _) <- readIORef ref1
                (e2, _) <- readIORef ref2
                p <- (random g :: IO Double)
                when (p < exp ((1/t1 - 1/t2) * (e1 - e2))) $ do
                    v1 <- readIORef ref1
                    v2 <- readIORef ref2
                    writeIORef ref1 v2
                    writeIORef ref2 v1
    readIORef (head chains)

------------------------------------------------------------------------
-- CSV loading
------------------------------------------------------------------------

splitOn :: Char -> String -> [String]
splitOn delim = foldr f [""]
  where
    f c (x:xs) | c == delim = "" : x : xs
               | otherwise  = (c:x) : xs
    f _ [] = []

loadCSV :: FilePath -> Int -> IO (V.Vector (Double, Double))
loadCSV path colIdx = do
    content <- readFile path
    let ls        = drop 1 $ lines content
        hasData l = let cols = splitOn ',' l
                    in  length cols > colIdx
                        && not (null (cols !! 1))
                        && not (null (cols !! colIdx))
        parseLine l = let cols = splitOn ',' l
                      in  (read (cols !! 1), read (cols !! colIdx))
    return $ V.fromList $ map parseLine $ filter hasData ls

------------------------------------------------------------------------
-- Log-spaced temperatures and main
------------------------------------------------------------------------

logTemps :: Int -> Double -> Double -> [Double]
logTemps n tMin tMax =
    [tMin * (tMax / tMin) ** (fromIntegral i / fromIntegral (n - 1))
    | i <- [0 .. n - 1]]

main :: IO ()
main = do
    pts <- loadCSV "../pinheiroTech/curves-data/all_curves.csv" 2
    let -- energy = optimise constants then evaluate
        ef tree = optimizeConsts pts tree >> energyF pts tree
        temps   = logTemps 15 0.001 20
    g <- getStdGen
    putStrLn "Searching with parallel tempering + constant optimisation..."
    printf "Points: %d  |  Chains: %d  |  Steps per chain: %d\n"
           (V.length pts) (length temps) (200 * 2000 :: Int)
    (e, best) <- parallelTempering @Double @Double
                     myBins myUns myLeaves
                     ef 6 200 2000 temps g
    printf "\nBest tree: %s\n\n" (show best)
    printf "Energy (MSE): %.8f\n\n" e
    printf "%-10s %-15s %-15s %-15s\n"
           ("x" :: String) ("found" :: String) ("target" :: String) ("error" :: String)
    V.mapM_ (\(x, y) -> do
        y' <- evalTree best x
        printf "%-10.4f %-15.6f %-15.6f %-15.6f\n" x y' y (abs (y' - y))
        ) pts
