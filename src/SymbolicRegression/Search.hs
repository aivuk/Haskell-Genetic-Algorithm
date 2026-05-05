{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}

module SymbolicRegression.Search
    ( SearchConfig(..)
    , defaultSearchConfig
    , logTemps
    , formatDiff
    , parallelTempering
    ) where

import Data.Typeable              (Typeable)
import Data.IORef
import Data.List                  (minimumBy)
import Data.Ord                   (comparing)
import Control.Monad              (forM, forM_, when, replicateM)
import Control.Concurrent.Async   (mapConcurrently)
import Data.Time.Clock            (getCurrentTime, diffUTCTime, NominalDiffTime)
import System.Random.Mersenne     (MTGen, random, getStdGen)
import Text.Printf
import SymbolicRegression.Tree
import SymbolicRegression.Mutate

-- | Configuration for a parallel-tempering search run.
data SearchConfig = SearchConfig
    { scMaxWallSeconds  :: Maybe Int      -- ^ Stop after this many seconds (Nothing = no limit)
    , scTargetEnergy    :: Maybe Double   -- ^ Stop when best energy drops below this
    , scMaxRounds       :: Int            -- ^ Hard cap on number of rounds
    , scStepsPerSwap    :: Int            -- ^ MC steps per chain between swap attempts
    , scLogEvery        :: Int            -- ^ Print progress every N rounds
    , scCheckpointFile  :: Maybe FilePath -- ^ Append checkpoints here (Nothing = disabled)
    , scCheckpointEvery :: Int            -- ^ Write checkpoint every N rounds
    , scTemps           :: [Double]       -- ^ Temperature ladder (ascending)
    , scDepth           :: Int            -- ^ Max tree depth for generation and mutation
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

-- | Logarithmically-spaced temperature ladder of length @n@ from @tMin@ to @tMax@.
logTemps :: Int -> Double -> Double -> [Double]
logTemps n tMin tMax =
    [ tMin * (tMax / tMin) ** (fromIntegral i / fromIntegral (n - 1))
    | i <- [0 .. n - 1] ]

-- | Format a 'NominalDiffTime' as @HH:MM:SS@.
formatDiff :: NominalDiffTime -> String
formatDiff d =
    let total = floor d :: Int
        h     = total `div` 3600
        m     = (total `mod` 3600) `div` 60
        s     = total `mod` 60
    in printf "%02d:%02d:%02d" h m s

-- Single Metropolis-Hastings step for one chain.
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

-- | Run parallel tempering across the temperature ladder in 'scTemps'.
-- Returns @(bestEnergy, bestTree, stopReason)@.
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

    let loop roundN = do
          now <- getCurrentTime
          let elapsed = diffUTCTime now startTime

          vals <- mapM readIORef chains
          let (bestE, bestT) = minimumBy (comparing fst) vals

          when (roundN `mod` scLogEvery cfg == 0) $
              printf "[round %d/%d | %s elapsed] best=%.3e\n"
                  roundN (scMaxRounds cfg) (formatDiff elapsed) bestE

          when (roundN `mod` scCheckpointEvery cfg == 0) $
              case scCheckpointFile cfg of
                  Just fp -> appendFile fp $
                      show roundN ++ "," ++ show bestE ++
                      ",\"" ++ show bestT ++ "\"\n"
                  Nothing -> return ()

          let timeLimitHit = case scMaxWallSeconds cfg of
                Just s  -> elapsed >= fromIntegral s
                Nothing -> False

          let energyTargetHit = case scTargetEnergy cfg of
                Just tgt -> bestE <= tgt
                Nothing  -> False

          if | roundN > scMaxRounds cfg ->
                 return (bestE, bestT, "max rounds reached")
             | timeLimitHit ->
                 return (bestE, bestT, "wall time limit reached")
             | energyTargetHit ->
                 return (bestE, bestT, "target energy reached")
             | otherwise -> do

                 _ <- mapConcurrently
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

                 loop (roundN + 1)

    loop 1
