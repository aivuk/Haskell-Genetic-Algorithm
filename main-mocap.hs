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
