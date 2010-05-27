module Main where

import System.Random.Mersenne
import Control.Monad.State
import GeneticProgramming
import qualified Data.Vector.Unboxed as V

type Vector = V.Vector Scalar
type Scalar = Double
type VS = Either Vector Scalar

-- Create binary functions on type Either

makeBin :: (Vector -> Vector -> Vector)
        -> (Scalar -> Scalar -> Scalar)
        -> (Vector -> Scalar -> Vector)
        -> (Scalar -> Vector -> Vector)
        -> (Vector -> Vector -> Scalar)
        -> (Scalar -> Scalar -> Vector)
        -> (Vector -> Scalar -> Scalar)
        -> (Scalar -> Vector -> Scalar)
        -> (VS -> VS -> VS, VS -> VS -> VS)

makeBin vvv sss vsv svv vvs ssv vss svs =
    (makeBinFst vvv sss vsv svv, makeBinSnd vvs ssv vss svs)

makeBinFst vvv sss vsv svv = f
  where
    f (Left v1)  (Left v2)  = Left  $ vvv v1 v2
    f (Right s1) (Right s2) = Right $ sss s1 s2
    f (Left v)   (Right s)  = Left  $ vsv v s
    f (Right s)  (Left v)   = Left  $ svv s v

makeBinSnd vvs ssv vss svs = g
  where
    g (Left v1)  (Left v2)  = Right $ vvs v1 v2
    g (Right s1) (Right s2) = Left  $ ssv s1 s2
    g (Left v)   (Right s)  = Right $ vss v s
    g (Right s)  (Left v)   = Right $ svs s v

-- Create Unary Functions on type Either

makeUn :: (Vector -> Vector)
       -> (Scalar -> Scalar)
       -> (Vector -> Scalar)
       -> (Scalar -> Vector)
       -> (VS -> VS, VS -> VS)

makeUn vv ss vs sv = (makeUnFst vv ss, makeUnSnd vs sv)

makeUnFst vv ss = f
  where
    f (Left v)  = Left  $ vv v
    f (Right s) = Right $ ss s

makeUnSnd vs sv = g
  where
    g (Left v)  = Right $ vs v
    g (Right s) = Left  $ sv s

-- Scalar Product

v1 <*> v2 = V.sum $ V.zipWith (*) v1 v2

norm v = (v <*> v)**(-1/(fromIntegral $ V.length v))

v *< s = V.map (s*) v
s >* v = v *< s

v1 <+> v2 = V.zipWith (+) v1 v2

bf1 = makeBin (<+>)
              (*)
              (*<)
              (>*)
              (<*>)
              (\x y -> V.fromList.(take 10) $ cycle [x,y])
              (\_ s -> s)
              (\s _ -> s)

-- Some functions

psqr = sqrt.abs
pexp = exp.(min 13)
plog = log.(max $ 10**(-17))
x `pdiv` y | abs y - 10**(-4) >= 0 = x/y
           | otherwise = 10**4 * signum x

-- Binary Functions

bl = [ ("(+)", (+)),
       ("(-)", (-)),
       ("(*)", (*)),
       ("Pdiv", pdiv) ]

-- Unary functions

ul = [ ("Psqr", psqr),
       ("Pexp", pexp),
       ("Plog", plog) ]

binArr = listToArray bl
unArr = listToArray ul

-- Perceptron

makePerceptron :: Vector            -- Weight Vector
               -> Scalar            -- Bias
               -> Vector -> Scalar  -- The Output Perceptron

makePerceptron w b = f
  where
    f x | w <*> x + b > 0  =  1
        | otherwise        =  0

perceptron = makePerceptron (V.fromList [1,2,3,4]) 0.3
