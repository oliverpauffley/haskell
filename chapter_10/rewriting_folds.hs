myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs


myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\x b -> b || f x) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (\x b -> b || x==a ) False


myElem' :: Eq a => a -> [a] -> Bool
myElem' a = myAny' (==a)

myReverse :: [a] -> [a]
myReverse = foldr (\x xs -> xs ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\y ys -> f y : ys) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []


squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) =  foldr (maxBy f) x xs
  where maxBy f a b = case f a b of
          GT -> a
          EQ -> a
          LT -> b
