module Artih2 where

add :: Int -> Int -> Int
add x y = x + y

addPf :: Int -> Int -> Int
addPf = (+)

addOne :: Int -> Int
addOne x = x + 1

addOnePF :: Int -> Int
addOnePF = (+1)


main :: IO ()
main = do
  print (0 :: Int)
  print (add 1 0)
  print (addOne 0)
  print (addOnePF 0)
  print ((addOne . addOne) 0)
  print ((addOnePF . addOne) 0)
  print ((addOne . addOne . addOne . negate . addOne) 0)
