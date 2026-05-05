{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad          (forM)
import System.Environment     (getArgs)
import System.IO              (hSetBuffering, stdout, BufferMode(..))
import Linear.V3              (V3(..), cross)

import SymbolicRegression.Tree
import SymbolicRegression.Search
import SymbolicRegression.Optimize

type Vec3 = V3 Double

------------------------------------------------------------------------
-- Safe math helpers
------------------------------------------------------------------------

safeRecip :: Double -> Double
safeRecip x = if abs x < 1e-8 then 1e8 else 1 / x

safeSqrt :: Double -> Double
safeSqrt x = sqrt (abs x + 1e-12)

------------------------------------------------------------------------
-- CSV helper
------------------------------------------------------------------------

splitOn :: Char -> String -> [String]
splitOn delim = foldr f [""]
  where
    f c (x:xs) | c == delim = "" : x : xs
               | otherwise  = (c : x) : xs
    f _ [] = []

------------------------------------------------------------------------
-- Finite-difference gradient helpers
------------------------------------------------------------------------

partialH :: PTree inp Double
         -> (inp -> Double)
         -> (inp -> Double -> inp)
         -> Double -> inp -> IO Double
partialH tree get set eps pt = do
    hp <- evalTree tree (set pt (get pt + eps))
    hm <- evalTree tree (set pt (get pt - eps))
    return ((hp - hm) / (2 * eps))

perturbV3 :: Int -> Double -> Vec3 -> Vec3
perturbV3 0 eps (V3 x y z) = V3 (x + eps) y z
perturbV3 1 eps (V3 x y z) = V3 x (y + eps) z
perturbV3 2 eps (V3 x y z) = V3 x y (z + eps)
perturbV3 _ _   v          = v

gradHV3 :: PTree inp Double
        -> (inp -> Vec3)
        -> (inp -> Vec3 -> inp)
        -> Double -> inp -> IO Vec3
gradHV3 tree get set eps pt = do
    let comp i = do
            vp <- evalTree tree (set pt (perturbV3 i   eps  (get pt)))
            vm <- evalTree tree (set pt (perturbV3 i (-eps) (get pt)))
            return ((vp - vm) / (2 * eps))
    dx <- comp 0; dy <- comp 1; dz <- comp 2
    return (V3 dx dy dz)

------------------------------------------------------------------------
-- System (a): 1D Harmonic Oscillator
------------------------------------------------------------------------

data HarmonicInput = HarmonicInput
    { hQ :: Double, hP :: Double
    , hDotQ :: Double, hDotP :: Double
    } deriving (Show)

harmonicBins :: [TBin]
harmonicBins =
    [ TBin "(+)" ((+) :: Double -> Double -> Double)
    , TBin "(*)" ((*) :: Double -> Double -> Double)
    , TBin "(-)" ((-) :: Double -> Double -> Double)
    ]

harmonicUns :: [TUn]
harmonicUns =
    [ TUn "sq"        ((\x -> x*x) :: Double -> Double)
    , TUn "neg"       (negate      :: Double -> Double)
    , TUn "safeRecip" (safeRecip   :: Double -> Double)
    ]

harmonicLeaves :: [TLeaf HarmonicInput]
harmonicLeaves =
    [ TLeaf "q" hQ
    , TLeaf "p" hP
    ]

sympLossHarmonic :: [HarmonicInput] -> PTree HarmonicInput Double -> IO Double
sympLossHarmonic pts tree = do
    losses <- forM pts $ \pt -> do
        dHdq <- partialH tree hQ (\pt' x -> pt' { hQ = x }) 1e-5 pt
        dHdp <- partialH tree hP (\pt' x -> pt' { hP = x }) 1e-5 pt
        let eq = hDotQ pt - dHdp
            ep = hDotP pt + dHdq
        return (eq*eq + ep*ep)
    return (sum losses / fromIntegral (length losses))

loadHarmonicCSV :: FilePath -> IO [HarmonicInput]
loadHarmonicCSV path = do
    content <- readFile path
    let dataLines = filter (not . null) $ drop 1 (lines content)
    return $ map parseLine dataLines
  where
    parseLine l =
        let [q,p,dq,dp] = map (read . filter (/= '\r')) (splitOn ',' l)
        in HarmonicInput q p dq dp

runHarmonic :: IO ()
runHarmonic = do
    putStrLn "=== System (a): 1D Harmonic Oscillator ==="
    pts <- loadHarmonicCSV "data/harmonic.csv"
    putStrLn $ "Loaded " ++ show (length pts) ++ " data points"
    let searchPts = take 300 pts
        cfg = defaultSearchConfig
                { scMaxWallSeconds = Just 270
                , scDepth          = 3
                , scTargetEnergy   = Just 1e-4
                , scCheckpointFile = Just "checkpoint_harmonic.csv"
                }
        energy tree = sympLossHarmonic searchPts tree
    (bestE, bestT, reason) <- parallelTempering harmonicBins harmonicUns harmonicLeaves energy cfg
    putStrLn $ "Stopped: " ++ reason
    putStrLn $ "Search energy (300-pt): " ++ show bestE
    optimizeConsts energy bestT
    optE  <- energy bestT
    fullE <- sympLossHarmonic pts bestT
    putStrLn $ "After const opt (300-pt): " ++ show optE
    putStrLn $ "Full dataset energy:      " ++ show fullE
    simplified <- foldConstants bestT >>= simplifyTree
    putStrLn $ "Best tree (simplified): " ++ show simplified

------------------------------------------------------------------------
-- System (b): 3D Rigid Body
------------------------------------------------------------------------

data RigidBodyInput = RigidBodyInput
    { rbL :: Vec3, rbDotL :: Vec3 } deriving (Show)

rigidBodyBins :: [TBin]
rigidBodyBins =
    [ TBin "(+)" ((+) :: Double -> Double -> Double)
    , TBin "(*)" ((*) :: Double -> Double -> Double)
    ]

rigidBodyUns :: [TUn]
rigidBodyUns = [ TUn "neg" (negate :: Double -> Double) ]

rigidBodyLeaves :: [TLeaf RigidBodyInput]
rigidBodyLeaves =
    [ TLeaf "lx"    (\inp -> let V3 x _ _ = rbL inp in x)
    , TLeaf "ly"    (\inp -> let V3 _ y _ = rbL inp in y)
    , TLeaf "lz"    (\inp -> let V3 _ _ z = rbL inp in z)
    , TLeaf "sq_lx" (\inp -> let V3 x _ _ = rbL inp in x*x)
    , TLeaf "sq_ly" (\inp -> let V3 _ y _ = rbL inp in y*y)
    , TLeaf "sq_lz" (\inp -> let V3 _ _ z = rbL inp in z*z)
    ]

sympLossRigidBody :: [RigidBodyInput] -> PTree RigidBodyInput Double -> IO Double
sympLossRigidBody pts tree = do
    losses <- forM pts $ \pt -> do
        gradH <- gradHV3 tree rbL (\pt' v -> pt' { rbL = v }) 1e-5 pt
        let diff = rbDotL pt - rbL pt `cross` gradH
            V3 ex ey ez = diff
        return (ex*ex + ey*ey + ez*ez)
    return (sum losses / fromIntegral (length losses))

loadRigidBodyCSV :: FilePath -> IO [RigidBodyInput]
loadRigidBodyCSV path = do
    content <- readFile path
    let dataLines = filter (not . null) $ drop 1 (lines content)
    return $ map parseLine dataLines
  where
    parseLine l =
        let [lx,ly,lz,dlx,dly,dlz] = map (read . filter (/= '\r')) (splitOn ',' l)
        in RigidBodyInput (V3 lx ly lz) (V3 dlx dly dlz)

runRigidBody :: IO ()
runRigidBody = do
    putStrLn "=== System (b): 3D Rigid Body ==="
    pts <- loadRigidBodyCSV "data/rigidbody.csv"
    putStrLn $ "Loaded " ++ show (length pts) ++ " data points"
    let searchPts = take 300 pts
        cfg = defaultSearchConfig
                { scMaxWallSeconds = Just 270
                , scDepth          = 3
                , scTargetEnergy   = Just 1e-4
                , scCheckpointFile = Just "checkpoint_rigidbody.csv"
                }
        energy tree = sympLossRigidBody searchPts tree
    (bestE, bestT, reason) <- parallelTempering rigidBodyBins rigidBodyUns rigidBodyLeaves energy cfg
    putStrLn $ "Stopped: " ++ reason
    putStrLn $ "Search energy (300-pt): " ++ show bestE
    optimizeConsts energy bestT
    optE  <- energy bestT
    fullE <- sympLossRigidBody pts bestT
    putStrLn $ "After const opt (300-pt): " ++ show optE
    putStrLn $ "Full dataset energy:      " ++ show fullE
    simplified <- foldConstants bestT >>= simplifyTree
    putStrLn $ "Best tree (simplified): " ++ show simplified

------------------------------------------------------------------------
-- System (c): Two-Body Gravity
------------------------------------------------------------------------

data TwoBodyInput = TwoBodyInput
    { tbQ1 :: Vec3, tbQ2 :: Vec3
    , tbP1 :: Vec3, tbP2 :: Vec3
    , tbDotQ1 :: Vec3, tbDotQ2 :: Vec3
    , tbDotP1 :: Vec3, tbDotP2 :: Vec3
    } deriving (Show)

twoBodyBins :: [TBin]
twoBodyBins =
    [ TBin "(+)" ((+) :: Double -> Double -> Double)
    , TBin "(*)" ((*) :: Double -> Double -> Double)
    ]

twoBodyUns :: [TUn]
twoBodyUns = [ TUn "neg" (negate :: Double -> Double) ]

-- Pre-computed scalar leaves keep the tree shallow enough for depth=3 to work.
-- gradHV3 perturbs the underlying Vec3 fields so finite-difference
-- gradients propagate correctly through these scalars.
twoBodyLeaves :: [TLeaf TwoBodyInput]
twoBodyLeaves =
    [ TLeaf "p1_sq" (\inp -> let V3 x y z = tbP1 inp in x*x+y*y+z*z)
    , TLeaf "p2_sq" (\inp -> let V3 x y z = tbP2 inp in x*x+y*y+z*z)
    , TLeaf "r_inv" (\inp ->
          let V3 dx dy dz = tbQ1 inp - tbQ2 inp
              r = sqrt (dx*dx+dy*dy+dz*dz)
          in if r < 1e-10 then 0.0 else 1.0/r)
    ]

sympLossTwoBody :: [TwoBodyInput] -> PTree TwoBodyInput Double -> IO Double
sympLossTwoBody pts tree = do
    losses <- forM pts $ \pt -> do
        dHdp1 <- gradHV3 tree tbP1 (\pt' v -> pt' { tbP1 = v }) 1e-5 pt
        dHdp2 <- gradHV3 tree tbP2 (\pt' v -> pt' { tbP2 = v }) 1e-5 pt
        dHdq1 <- gradHV3 tree tbQ1 (\pt' v -> pt' { tbQ1 = v }) 1e-5 pt
        dHdq2 <- gradHV3 tree tbQ2 (\pt' v -> pt' { tbQ2 = v }) 1e-5 pt
        let eq1 = tbDotQ1 pt - dHdp1
            eq2 = tbDotQ2 pt - dHdp2
            ep1 = tbDotP1 pt + dHdq1
            ep2 = tbDotP2 pt + dHdq2
            norm2 (V3 a b c) = a*a + b*b + c*c
        return (norm2 eq1 + norm2 eq2 + norm2 ep1 + norm2 ep2)
    return (sum losses / fromIntegral (length losses))

loadTwoBodyCSV :: FilePath -> IO [TwoBodyInput]
loadTwoBodyCSV path = do
    content <- readFile path
    let dataLines = filter (not . null) $ drop 1 (lines content)
    return $ map parseLine dataLines
  where
    parseLine l =
        let vs = map (read . filter (/= '\r')) (splitOn ',' l)
            [q1x,q1y,q1z,q2x,q2y,q2z,
             p1x,p1y,p1z,p2x,p2y,p2z,
             dq1x,dq1y,dq1z,dq2x,dq2y,dq2z,
             dp1x,dp1y,dp1z,dp2x,dp2y,dp2z] = vs
        in TwoBodyInput
             { tbQ1    = V3 q1x q1y q1z,  tbQ2    = V3 q2x q2y q2z
             , tbP1    = V3 p1x p1y p1z,  tbP2    = V3 p2x p2y p2z
             , tbDotQ1 = V3 dq1x dq1y dq1z, tbDotQ2 = V3 dq2x dq2y dq2z
             , tbDotP1 = V3 dp1x dp1y dp1z, tbDotP2 = V3 dp2x dp2y dp2z
             }

runTwoBody :: IO ()
runTwoBody = do
    putStrLn "=== System (c): Two-Body Gravity ==="
    pts <- loadTwoBodyCSV "data/twobody.csv"
    putStrLn $ "Loaded " ++ show (length pts) ++ " data points"
    let searchPts = [pts !! i | i <- [0,20..2999]]
        cfg = defaultSearchConfig
                { scMaxWallSeconds = Just 270
                , scDepth          = 3
                , scStepsPerSwap   = 200
                , scTargetEnergy   = Just 1e-3
                , scCheckpointFile = Just "checkpoint_twobody.csv"
                }
        energy tree = sympLossTwoBody searchPts tree
    (bestE, bestT, reason) <- parallelTempering twoBodyBins twoBodyUns twoBodyLeaves energy cfg
    putStrLn $ "Stopped: " ++ reason
    putStrLn $ "Search energy (150-pt): " ++ show bestE
    optimizeConsts energy bestT
    optE  <- energy bestT
    fullE <- sympLossTwoBody pts bestT
    putStrLn $ "After const opt (150-pt): " ++ show optE
    putStrLn $ "Full dataset energy:      " ++ show fullE
    simplified <- foldConstants bestT >>= simplifyTree
    putStrLn $ "Best tree (simplified): " ++ show simplified

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case args of
        ("harmonic":_)  -> runHarmonic
        ("rigidbody":_) -> runRigidBody
        ("nbody":_)     -> runTwoBody
        _ -> putStrLn "Usage: symbolic-regression-hamilton harmonic|rigidbody|nbody"
