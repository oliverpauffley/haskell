module Main where

import           Criterion.Main
import qualified Data.Vector         as B
import qualified Data.Vector.Unboxed as UB

unboxedVector :: Int -> UB.Vector Int
unboxedVector n = UB.fromList [0..n]

boxedVector :: Int -> B.Vector Int
boxedVector n = B.fromList [0..n]

main :: IO ()
main = defaultMain
  [
    bench "boxed" $ whnf boxedVector 9998
  , bench "unboxed" $ whnf unboxedVector 9998
  ]
