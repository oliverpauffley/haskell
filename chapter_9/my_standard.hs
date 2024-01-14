-- returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs
--myOr = foldr (||) False


--myAny returns True if a -> Bool applied to any of the values returns true
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

-- returns true if the list contains the element given.
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem y (x:xs) = y == x || myElem y xs

myElemWithAny :: Eq a => a -> [a] -> Bool
myElemWithAny a = myAny (a==)


-- reverse a list or string
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = last xs :  myReverse (init xs)


-- squish flattens a list of lists into a single list
squish :: [[a]] -> [a]
squish (x:xs)=  x ++ squish xs


-- maps a function over a list and concatenates the result
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = concat (map f xs)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


myMaxiumumBy :: (a -> a -> Ordering) -> [a] -> a
myMaxiumumBy cmp (x:xs) = go cmp xs x
  where go c [] max = max
        go c (x:xs) max
          | c max x == LT = go c xs x
          | otherwise = go c xs max

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp (x:xs) = go cmp xs x
  where go c [] min = min
        go c (x:xs) min
          | c min x == GT = go c xs x
          | otherwise = go c xs min

myMax :: (Ord a) => [a] -> a
myMax = myMaxiumumBy compare

myMin :: (Ord a) => [a] -> a
myMin = myMinimumBy compare
