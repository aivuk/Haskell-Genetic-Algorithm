{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SymbolicRegression.Mutate
    ( genPTree
    , mutatePTree
    ) where

import Data.Typeable          (Typeable, eqT)
import Data.Type.Equality     ((:~:)(Refl))
import Data.Proxy             (Proxy(..))
import Data.Maybe             (mapMaybe)
import Data.Array             (listArray, Array, bounds, (!))
import Data.IORef             (newIORef)
import System.Random.Mersenne (MTGen, random)
import SymbolicRegression.Tree

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

-- | Generate a random expression tree of the given max depth.
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

-- | Replace one randomly chosen subtree with a freshly generated tree.
mutatePTree :: forall a b. (Typeable a, Typeable b)
            => PTree a b
            -> [TBin] -> [TUn] -> [TLeaf a]
            -> Int -> MTGen -> IO (PTree a b)
mutatePTree tree bins uns leaves maxDepth g = do
    node <- fmap (`mod` sizeP tree) (random g :: IO Int)
    go tree node
  where
    go :: forall c. Typeable c => PTree a c -> Int -> IO (PTree a c)
    go _ 0               = genPTree @a @c bins uns leaves maxDepth g
    go t@(PV _ _)    _   = return t
    go t@(PConst _)  _   = return t
    go t@(PConstV _) _   = return t
    go t@(PConstQ _) _   = return t
    go (PUn name f sub) n =
        PUn name f <$> go sub (n - 1)
    go (PBin name f l r) n =
        let sl = sizeP l
        in if n - 1 < sl
           then (\l' -> PBin name f l' r) <$> go l (n - 1)
           else (\r' -> PBin name f l r') <$> go r (n - 1 - sl)
