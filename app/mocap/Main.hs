{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Maybe                 (mapMaybe)
import qualified Data.Vector      as V
import Text.Printf
import Control.Monad              (forM)
import System.IO                  (hSetBuffering, stdout, BufferMode(..))
import Linear.V3                  (V3(..))
import Linear.Quaternion          (Quaternion(..))
import Linear.Metric              (dot, norm)
import Linear.Vector              ((*^))

import SymbolicRegression.Tree
import SymbolicRegression.Search
import SymbolicRegression.Optimize

------------------------------------------------------------------------
-- Domain types
------------------------------------------------------------------------

type Vec3 = V3 Double
type Quat = Quaternion Double

data MocapInput = MocapInput
    { miOmega :: Vec3
    , miDt    :: Double
    } deriving (Show)

------------------------------------------------------------------------
-- Quaternion exponential: exp_q(v) = cos(|v|) + sin(|v|)/|v| * v
------------------------------------------------------------------------

expQ :: Vec3 -> Quat
expQ v =
    let h = norm v
    in if h < 1e-10
       then Quaternion 1 (V3 0 0 0)
       else Quaternion (cos h) ((sin h / h) *^ v)

------------------------------------------------------------------------
-- Function pool
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
-- Data loading
------------------------------------------------------------------------

splitOn :: Char -> String -> [String]
splitOn delim = foldr f [""]
  where
    f c (x:xs) | c == delim = "" : x : xs
               | otherwise  = (c:x) : xs
    f _ [] = []

-- CSV columns: omega_x, omega_y, omega_z, dt, dq_w, dq_x, dq_y, dq_z
loadMocapCSV :: FilePath -> IO (V.Vector (Vec3, Double, Quat))
loadMocapCSV path = do
    content <- readFile path
    let ls = drop 1 (lines content)
        parseLine l =
            let cols = splitOn ',' l
            in case map (read :: String -> Double) cols of
                [ox, oy, oz, dt, dw, dx, dy, dz] ->
                    Just (V3 ox oy oz, dt, Quaternion dw (V3 dx dy dz))
                _ -> Nothing
    return $ V.fromList $ mapMaybe parseLine ls

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
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    let csvPath = "data/mocap_sample.csv"
    putStrLn $ "Loading " ++ csvPath ++ " ..."
    allPts <- loadMocapCSV csvPath

    let pts = V.take 300 allPts
    printf "Loaded %d rows, using %d for search\n" (V.length allPts) (V.length pts)

    let ef  tree = energyMocap pts tree
        cfg = defaultSearchConfig
                { scMaxWallSeconds = Just 3600
                , scTargetEnergy   = Just 5e-5
                , scDepth          = 3
                , scCheckpointFile = Just "checkpoint_mocap.csv"
                }

    printf "Starting search: %d chains, max %d rounds, depth %d\n"
        (length (scTemps cfg)) (scMaxRounds cfg) (scDepth cfg)

    (e, best, reason) <- parallelTempering @MocapInput @Quat
                             mocapBins mocapUns mocapLeaves ef cfg

    optimizeConsts (energyMocap allPts) best
    eFinal    <- energyMocap allPts best
    simplified <- foldConstants best
    normalized <- simplifyTree simplified

    putStrLn $ "\nStopped: " ++ reason
    printf "Search energy (300 pts):      %.8f\n" e
    printf "Final energy (all %d pts):  %.8f\n" (V.length allPts) eFinal
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
