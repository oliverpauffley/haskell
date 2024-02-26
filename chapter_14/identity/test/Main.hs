module Main (main) where

import           Data.List       (sort)
import           Test.QuickCheck

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, _)       = (Just y, x>=y)

listProperty :: [Int] -> Bool
listProperty xs = listOrdered (sort xs)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
  x + (y +z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
  x + y == y + x

multiplicationAssociative :: Int -> Int -> Int -> Bool
multiplicationAssociative x y z =
  x * (y * z) == (x * y ) * z

multiplicationCommutative :: Int -> Int -> Bool
multiplicationCommutative x y =
  x * y == y * x

divProp :: Int -> (NonZero Int) -> Bool
divProp x (NonZero y ) =
  (quot x y ) * y + (rem x y) == x

divProp' :: Int -> (NonZero Int) -> Bool
divProp' x (NonZero y) =
  (div x y ) * y + (mod x y) == x

listDoubleReverse :: [Int] -> Bool
listDoubleReverse x =
 reverse (reverse x) == id x


twice :: (a -> a) -> a -> a
twice l = l . l

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

f :: [Int] -> Bool
f x = (sort x == twice sort x) &&
  (sort x == fourTimes sort x)

data Fool =
  Fulse
  | Frue
  deriving (Eq, Show)

foolGen :: Gen Fool
foolGen =
 oneof [return Fulse, return Frue]

foolGen' :: Gen Fool
foolGen' =
  frequency [(2, return Fulse),(1, return Frue)]

main :: IO ()
main = do
  (quickCheck .verbose) listProperty
  (quickCheck .verbose) plusAssociative
  (quickCheck .verbose) plusCommutative
  (quickCheck .verbose) multiplicationAssociative
  (quickCheck .verbose) multiplicationCommutative
  (quickCheck .verbose) divProp
  (quickCheck .verbose) divProp'
  (quickCheck .verbose) listDoubleReverse
  (quickCheck. verbose) f
  sample foolGen
  sample foolGen'
