lefts' :: [Either a b] -> [a]
lefts' = foldr picker []
  where picker (Left a) xs  = a: xs
        picker (Right _) xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr picker []
  where picker (Left _) xs  = xs
        picker (Right b) xs = b : xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

partitionEithers'' :: [Either a b] -> ([a], [b])
partitionEithers'' = go [] []
  where go as bs (Left x:xs)  = go (x:as) bs xs
        go as bs (Right x:xs) = go as (x:bs) xs
        go as bs []           = (as, bs)

-- one I found online, nice to use foldr but I think mine works too!
partitionEithers''' :: [Either a b] -> ([a], [b])
partitionEithers''' = foldr part ([], [])
  where part (Left x)  (ls, rs) = (x:ls, rs)
        part (Right x) (ls, rs) = (ls, x:rs)


eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left a)  =  f a
either' _ h (Right b) =  h b

eitherMaybe'' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
