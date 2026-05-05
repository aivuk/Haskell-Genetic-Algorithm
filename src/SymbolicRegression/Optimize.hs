module SymbolicRegression.Optimize
    ( goldenSearchIO
    , optimizeConsts
    ) where

import Data.IORef
import Control.Monad          (forM_)
import SymbolicRegression.Tree

-- | 1-D golden-section minimisation of @f@ on @[lo, hi]@ in @n@ steps.
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

-- | Coordinate-descent constant optimisation: 3 passes of golden-section
-- search over every 'PConst' leaf in the tree.
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
