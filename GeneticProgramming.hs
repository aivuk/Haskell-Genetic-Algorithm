module GeneticProgramming 
    ( Tree
    , NodeType
    -- Functions
    , listToArray
    , treeToFunc
    , mcPass 
    , printDot
    ) where

import System.Random.Mersenne
import Data.Array
import Control.Applicative
import Control.Monad.State
import Data.IORef
import Text.Printf
import qualified Data.Vector.Unboxed as V

-- Binary Tree representing a program

data Tree a = Bin (String, (a -> a -> a)) (Tree a) (Tree a)
            | Un (String, (a -> a)) (Tree a)
            | V

-- Node Type

data NodeType = BinT
              | UnT
              | VT

-- A Tree Associated with a specific set of binary and unary functions

data TreeType a = T { tree :: Tree a, 
                      binA :: BinArray a,
                      unA :: UnArray a }

-- Convert a Tree to a function

treeToFunc :: Tree a -> a -> a
treeToFunc (Bin f l r) =  (snd f) <$> treeToFunc l <*> treeToFunc r
treeToFunc (Un f u) = (snd f).(treeToFunc u)
treeToFunc V = id

type RandomTree a = StateT MTGen IO (Tree a)
type BinArray a = Array Int (String, a -> a -> a)
type UnArray a = Array Int (String, a -> a)
type EnergyFunction a = (a -> a) -> Double

-- Print the Tree in a Nice Way

instance Show (Tree a) where
    show (Bin f l r) = 
            "Bin " ++ (fst f) ++ " (" ++ show l ++ ") (" ++ show r ++ ")"
    show (Un f u) = 
            "Un " ++ (fst f) ++ " (" ++ show u ++ ")"
    show V = 
            "x"

-- Convert a list to an Array

listToArray :: [a] -> Array Int a
listToArray l = listArray (0, length l) l

-- Select a random element from an Array

randElem :: Array Int a -> MTGen -> IO a
randElem a g = return.(a!) =<< ri
  where 
    ri = return.(`mod` len) =<< random g
    len = snd $ bounds a

-- Select a random Node type

randNodeType :: MTGen -> IO NodeType
randNodeType g = randElem nodeTypeArr g
  where 
    nodeTypeArr = listArray (0, 2) [ BinT, UnT, VT ]

-- Calculate the number of nodes in a tree

sizeTree :: Tree a -> Int
sizeTree (Bin _ l r) = 1 + sizeTree l + sizeTree r
sizeTree (Un _ u) = 1 + sizeTree u
sizeTree V = 1

-- Save Random Tree

createRandomTree :: Int 
                 -> BinArray a 
                 -> UnArray a 
                 -> EnergyFunction a
                 -> StateT MTGen IO (IORef (Double, Tree a))

createRandomTree level binArr unArr energy_f = do
    g <- get
    randomTree <- genRandTree level binArr unArr
    let energy = energy_f (treeToFunc randomTree)
    lift $ newIORef (energy, randomTree)

-- Generate a Random Tree of maximum depth 'level'

genRandTree :: Int           -- ^ Maximem depth of the new tree
            -> BinArray a    -- ^ Array of Binary functions
            -> UnArray a     -- ^ Array of Unary functions
            -> RandomTree a  -- ^ Generated Random Tree

genRandTree 0 _ _ = return V
genRandTree level binArr unArr = do
    g <- get
    nodeType <- lift $ randNodeType g
    case nodeType of BinT -> do
                                binFunc <- lift $ randElem binArr g
                                left <- genRandTree (level - 1) binArr unArr
                                right <- genRandTree (level - 1) binArr unArr
                                return $ Bin binFunc left right
                     UnT  ->do
                                unFunc <- lift $ randElem unArr g
                                un <- genRandTree (level - 1) binArr unArr
                                return $ Un unFunc un
                     VT   -> do return V

-- Number of nodes in a given depth of a Tree

nodesInLevel :: Tree a  -- ^ Tree to be analized
             -> Int     -- ^ Depth
             -> Int     -- ^ Number of nodes in one specific depth

nodesInLevel tree level = nl tree 1 
  where 
    nl t i | i < level = 
                case t of Bin _ l r -> nl l (i + 1) + nl r (i + 1)
                          Un _ u -> nl u (i + 1)
                          V -> 0
           | i == level = 1
           | otherwise = error "Level must be bigger than 1"

-- Return the maximum depth of a Tree

depth :: Tree a -> Int
depth tree = depth' tree 1 
  where 
    depth' (Bin _ l r) d = max (depth' l $ d + 1) (depth' r $ d + 1)
    depth' (Un _ u) d = depth' u (d + 1)
    depth' V d = d

-- Apply a function returning a Tree in a given level and position 
-- (Not the fastest solution, but works!)

applyInNodeLevel :: (Tree a -> Tree a) 
                 -> Tree a 
                 -> Int 
                 -> Int 
                 -> Tree a

applyInNodeLevel f tree level node = appNL tree 0 1
  where 
    appNL t ni li | li == level = f t
                  | otherwise = 
            case t of Un uf u -> Un uf (appNL u ni (li + 1))
                      V -> V
                      Bin bf l r -> if (ni + nleft) > node
                                      then Bin bf (appNL l ni $ li + 1) r
                                      else Bin bf l (appNL r nleft $ li + 1)
                            where 
                                nleft = nodesInLevel l (level - li)

-- Insert a tree in a given level and position 
--  (Not the fastest solution, but works!)

insertTreeInNodeLevel :: Tree a 
                      -> Tree a 
                      -> Int 
                      -> Int 
                      -> Tree a

insertTreeInNodeLevel insTree tree level node = 
    applyInNodeLevel (const insTree) tree level node

-- Apply a function returning a Tree in a given Node

applyInNode :: (Tree a -> Tree a)  -- ^ Function that modifies a tree
            -> Tree a              -- ^ Tree to be modified
            -> Int                 -- ^ Node number to apply the function
            -> Tree a              -- ^ Modified Tree

applyInNode f tree node = appN tree 0
  where 
    appN t ni | ni == node = f t
              | otherwise = 
            case t of Un uf u -> Un uf (appN u $ ni + 1)
                      V -> V
                      Bin bf l r -> if (ni + nleft) >= node
                                      then Bin bf (appN l $ ni + 1) r
                                      else Bin bf l (appN r $ ni + nleft + 1)
                                            where 
                                                nleft = sizeTree l

-- Insert a Tree in a given node

insertTreeInNode :: Tree a  -- ^ Tree to be inserted
                 -> Tree a  -- ^ Tree that will be modified
                 -> Int     -- ^ Number of node that will have the insertion
                 -> Tree a  -- ^ Modified Tree

insertTreeInNode itree tree node = applyInNode (const itree) tree node

-- Insert a Random Tree with maximum depth d_k

mutateTree :: Tree a        -- Tree to be modified
           -> BinArray a    -- Array of Binary functions
           -> UnArray a     -- Array of Unary functions
           -> Int           -- Maximum depth of the random generated 
                            --  segment to be inserted
           -> RandomTree a  -- Random created tree

mutateTree tree binArr unArr d_k = do 
    g <- get
    randomNode <- lift $ return.(`mod` size) =<< random g
    randomTree <- genRandTree d_k binArr unArr
    return $ insertTreeInNode randomTree tree randomNode
      where
        size = sizeTree tree

-- Given a set of points and a grid, return the energy function of a program 'f'

{--
energyF :: V.Vector (Double, Double) 
        -> V.Vector Double 
        -> (Double -> Double) 
        -> Double

energyF points grid f = quadratic_error/p + roughness/m
  where 
    p = fromIntegral $ V.length points :: Double
    m = fromIntegral $ V.length grid :: Double
    quadratic_error = V.foldl (\s (x,y) -> s + (y - f x)**2) 0 points
    g = (grid V.!)
    gridDiff =  
    roughness = V.foldl (\s x -> abs $ f (g $ x + 1) - f (g x)) 0 $ 
                                                        V.enumFromStepN 1 1 (floor m - 1)
--}

-- Calculate a Monte Carlo step

mcPass :: IORef (Double, Tree a)  -- ^ A tree with his respective energy
       -> BinArray a              -- ^ Array of binary Functions
       -> UnArray a               -- ^ Array of Unary Functions
       -> Double                  -- ^ 1/T
       -> EnergyFunction a        -- ^ Calculate the energy of a Tree function
       -> (Int, Int) 
       -> StateT MTGen IO (Int, Int)

mcPass treeRef binArr unArr beta energy_f (a, d_k) = do
    g <- get
    (energy, tree) <- lift $ readIORef treeRef
    randDepth <- lift $ return.(`mod` d_k) =<< (random g :: IO Int)
    randTree <- mutateTree tree binArr unArr randDepth
    -- The maximum depth is 15
    if depth randTree <= 15
      then do
        let new_energy = energy_f (treeToFunc randTree)
            delta_e = new_energy - energy
        -- just update the tree if energy is lowered
        if delta_e < 0   
          then do
               lift $ writeIORef treeRef (new_energy, randTree)
               return (a + 1, d_k)
          else do 
            p <- lift $ (random g :: IO Double)
            if p < exp (-beta*delta_e) 
              then do
                lift $ writeIORef treeRef (new_energy, randTree)
                return (a + 1, d_k)
              else return (a, d_k)
      else return (a, d_k)

-- Calculate N MC steps

nMcSteps :: IORef (Double, Tree a)
         -> BinArray a 
         -> UnArray a
         -> Double
         -> EnergyFunction a
         -> Int
         -> Int
         -> StateT MTGen IO (Int, Int)

nMcSteps tr ba ua b ef n d_k_i = do
    let mcPass' (a, d_k) i | i `mod` 10 == 0 = 
                               if a == 0
                                 then mcPass tr ba ua b ef (0, d_k_i + 1)
                                 else  mcPass tr ba ua b ef (a, d_k)
                           | a == 5 = mcPass tr ba ua b ef (0, d_k - 1)
                           | otherwise = mcPass tr ba ua b ef (a, d_k)
    foldM mcPass' (0, 5) [1..n]

-- Convert a Tree to Graphviz DOT format

printDot :: Tree a -> IO ()
printDot tree = do
    printf "digraph test {\n\
           \\tmargin = \"0\"\n\
           \\tpage = \"0.0,0.0\"\n\
           \\tsize = \"0.0,0.0\"\n\
           \\trotate = \"0\"\n\
           \\tratio = \"fill\"\n"
    toDot tree 1
    printf "}"
      where
        toDot (Bin f l r) i = do 
            printf "\t%d [level = \"%s\"]" i (fst f)
            let st = sizeTree l
            printf "\t %d -> %d\n\t %d -> %d\n" i (i + 1) i (i + 1 + st)
            toDot l (i + 1)
            toDot r (1 + i + st)
        toDot (Un f u) i = do 
                printf "\t%d [label = \"%d\"]\n" i (fst f)
                printf "\t%d -> %d\n" i (i + 1)
                toDot u (i + 1)
        toDot V i = printf "\t%d [label = \"x\"]\n" i


