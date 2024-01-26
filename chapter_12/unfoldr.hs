myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe(a,b))
          -> b
          -> [a]
myUnfoldr f b = unfold (f b)
  where unfold Nothing      = []
        unfold (Just (a,b)) = a : myUnfoldr f b

-- or with cases
myUnfoldr' :: (b -> Maybe(a,b))
          -> b
          -> [a]
myUnfoldr' f b = case f b of
   Nothing    -> []
   Just (a,b) -> a : myUnfoldr f b


myIterate' :: (a -> a) -> a -> [a]
myIterate' f  = myUnfoldr (\a -> Just (a, f a))
