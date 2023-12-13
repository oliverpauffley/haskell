tensDigit x = d
  where xLast = fst (divMod x 10)
        d = snd (divMod xLast 10)

hundredsDigit x = d2
  where d = fst (divMod x 100)
        d2 = snd (divMod d 10)

foldBool :: a -> a -> Bool -> a
foldBool x y z  =  case z of
  True  -> y
  False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
 | z  = x
 | otherwise  = y


g :: (a -> b) -> (a,c) -> (b, c)
g f (a, c) =  (f a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show


roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2  = read . show
