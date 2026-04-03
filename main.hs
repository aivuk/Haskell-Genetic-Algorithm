module Main where

import Prelude hiding ((<*>))
import System.Random.Mersenne
import Control.Monad.State
import GeneticProgramming
import Data.IORef
import Data.List
import qualified Data.Vector.Unboxed as V
import Text.Printf

type Vector = V.Vector Scalar
type Scalar = Double
type VS = Either Vector Scalar

binVVV :: [Vector -> Vector -> Vector]
binVVV = [(<+>)] 

binVVS :: [Vector -> Vector -> Scalar]
binVVS = [(<*>)]

binVSV :: [Vector -> Scalar -> Vector]
binVSV = [(*<)] 

binSVV :: [Scalar -> Vector -> Vector]
binSVV = [(>*)]

binSSS :: [Scalar -> Scalar -> Scalar]
binSSS = [(*), (+)]

binSSV = []
binSVS = []
binVSS = []

unVV :: [Vector -> Vector]
unVV = [((-1) >*)]

unVS :: [Vector -> Scalar]
unVS = [norm]

unSS :: [Scalar -> Scalar]
unSS = [psqr, plog, pexp]

unSV = []

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

treeToLatex :: Tree Double -> String
treeToLatex V = "x"
treeToLatex (Un (name, _) u) = case name of
    "Psqr" -> "\\sqrt{\\left|" ++ treeToLatex u ++ "\\right|}"
    "Pexp" -> "e^{" ++ treeToLatex u ++ "}"
    "Plog" -> "\\ln\\left(" ++ treeToLatex u ++ "\\right)"
    _      -> name ++ "\\left(" ++ treeToLatex u ++ "\\right)"
treeToLatex (Bin (name, _) l r) = case name of
    "(+)"  -> "\\left(" ++ treeToLatex l ++ " + " ++ treeToLatex r ++ "\\right)"
    "(-)"  -> "\\left(" ++ treeToLatex l ++ " - " ++ treeToLatex r ++ "\\right)"
    "(*)"  -> treeToLatex l ++ " \\cdot " ++ treeToLatex r
    "Pdiv" -> "\\frac{" ++ treeToLatex l ++ "}{" ++ treeToLatex r ++ "}"
    _      -> name ++ "\\left(" ++ treeToLatex l ++ ", " ++ treeToLatex r ++ "\\right)"

-- Perceptron

makePerceptron :: Vector            -- Weight Vector
               -> Scalar            -- Bias
               -> Vector -> Scalar  -- The Output Perceptron

makePerceptron w b = f
  where
    f x | w <*> x + b > 0  =  1
        | otherwise        =  0

perceptron = makePerceptron (V.fromList [1,2,3,4]) 0.3

splitOn :: Char -> String -> [String]
splitOn delim = foldr f [""]
  where
    f c (x:xs) | c == delim = "" : x : xs
               | otherwise  = (c:x) : xs
    f _ [] = []

loadCSV :: FilePath -> Int -> IO (V.Vector (Double, Double))
loadCSV path colIdx = do
    content <- readFile path
    let ls         = drop 1 $ lines content
        hasData l  = let cols = splitOn ',' l
                     in  length cols > colIdx && not (null (cols !! 1)) && not (null (cols !! colIdx))
        parseLine l = let cols = splitOn ',' l
                      in  (read (cols !! 1), read (cols !! colIdx))
    return $ V.fromList $ map parseLine $ filter hasData ls

main = do
    csvData <- loadCSV "../pinheiroTech/curves-data/all_curves.csv" 2
    let timeVec = V.map fst csvData
        ef      = energyF csvData timeVec
    g <- getStdGen
    (_,_,ts) <- fmap unzip3 $ evalStateT (parallelMc 15 binArr unArr ef (10**(-4)) 100 (10^5) 10 30 5) g
    trees <- mapM readIORef ts
    let tmin = snd $ minimumBy (\x y -> compare (fst x) (fst y)) trees
    putStrLn $ "Best tree: " ++ show tmin
    putStrLn ""
    putStrLn $ "LaTeX: $" ++ treeToLatex tmin ++ "$"
    putStrLn ""
    printf "%-10s %-15s %-15s %-15s\n" ("x"::String) ("found"::String) ("target"::String) ("error"::String)
    V.mapM_ (\(x, y) -> printf "%-10.4f %-15.6f %-15.6f %-15.6f\n" x (treeToFunc tmin x) y (abs (treeToFunc tmin x - y))) csvData


