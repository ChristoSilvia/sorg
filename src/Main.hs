{-# LANGUAGE CPP, BangPatterns, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import System.Environment

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I

type Weight = Int32
type Graph = Array DIM2 Weight

step :: Acc (Scalar Int) -> Acc Graph -> Acc Graph
step k g = generate (shape g) sp
  where
    k' = the k

    sp :: Exp DIM2 -> Exp Weight
    sp ix = let (Z :. i :. j) = unlift ix
            in min (g ! (index2 i j))
	             (g ! (index2 i k') + g ! (index2 k' j))

shortestPathsAcc :: Int -> Acc Graph -> Acc Graph
shortestPathsAcc n g0 = foldl1 (>->) steps g0
  where
    steps :: [Acc Graph -> Acc Graph]
    steps = [step (unit (constant k)) | k <- [0 .. n-1]]

shortestPaths :: Graph -> Graph
shortestPaths g0 = run (shortestPathsAcc n (use g0))
  where
    Z :. _ :. n = arrayShape g0

toAdjMatrix :: [[Weight]] -> Graph
toAdjMatrix xs = fromList (Z :. nrow :. ncol) (concat xs)
  where
    nrow = length xs
    ncol = length . head $ xs

testGraph :: Graph
testGraph = toAdjMatrix $
  [[   0, 999, 999,  13, 999, 999],
   [ 999,   0, 999, 999,   4,   9],
   [  11, 999,   0, 999, 999, 999],
   [ 999,   3, 999,   0, 999,   7],
   [  15,   5, 999,   1,   0, 999],
   [  11, 999, 999,  14, 999,   0]]

main :: IO ()
main = do
  (n:_) <- fmap (fmap read) getArgs
  print (run (let g :: Acc Graph
	          g = generate (constant (Z:.n:.n) :: Exp DIM2) f

                  f :: Exp DIM2 -> Exp Weight
		  f ix = let i,j :: Exp Int
	                     Z:.i:.j = unlift ix
			 in
			   A.fromIntegral j + (A.fromIntegral i) * constant (Prelude.fromIntegral n)
	      in (shortestPathsAcc n g)))
