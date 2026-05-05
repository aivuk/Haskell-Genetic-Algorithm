{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SymbolicRegression.Tree
    ( -- * Expression tree
      PTree(..)
    , evalTree
    , sizeP
    , collectConsts
    , hasLeaf
    , foldConstants
    , simplifyTree
      -- * Pretty-printing helpers
    , showVec
    , showQuat
      -- * Open function pool
    , TBin(..)
    , TUn(..)
    , TLeaf(..)
      -- * Runtime type matching
    , MatchedBin(..)
    , MatchedUn(..)
    , matchBin
    , matchUn
    , matchLeaf
    ) where

import Data.Typeable      (Typeable, eqT)
import Data.Type.Equality ((:~:)(Refl))
import Data.Proxy         (Proxy(..))
import Data.IORef
import System.IO.Unsafe   (unsafePerformIO)
import Linear.V3          (V3(..))
import Linear.Quaternion  (Quaternion(..))

type Vec3 = V3 Double
type Quat = Quaternion Double

-- | Typed expression tree, indexed by input type @a@ and output type @b@.
-- The GADT encoding prevents ill-typed trees from being constructed.
data PTree a b where
    PBin    :: (Typeable x, Typeable y)
            => String -> (x -> y -> b) -> PTree a x -> PTree a y -> PTree a b
    PUn     :: Typeable x
            => String -> (x -> b) -> PTree a x -> PTree a b
    PV      :: String -> (a -> b) -> PTree a b
    PConst  :: IORef Double -> PTree a Double
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

-- | True if the tree has at least one 'PV' leaf (depends on input).
hasLeaf :: PTree a b -> Bool
hasLeaf (PBin _ _ l r) = hasLeaf l || hasLeaf r
hasLeaf (PUn  _ _ u)   = hasLeaf u
hasLeaf (PV   _ _)     = True
hasLeaf _              = False

-- | Replace constant subtrees (no 'PV' nodes) with a single evaluated node.
foldConstants :: forall a b. Typeable b => PTree a b -> IO (PTree a b)
foldConstants t
    | not (hasLeaf t) = do
        let dummy = error "foldConstants: impossible"
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

-- | Algebraic normalization: fold @neg@ into adjacent 'PConst' values.
simplifyTree :: forall a b. Typeable b => PTree a b -> IO (PTree a b)
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
            PConst ref -> do v <- readIORef ref
                             ref' <- newIORef (negate v)
                             return (PConst ref')
            _          -> return (PUn n f u')
          _ -> return (PUn n f u')
    step other = return other

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
