
-- returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs
--myOr = foldr (||) False


--myAny returns True if a -> Bool applied to any of the values returns true
myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined
