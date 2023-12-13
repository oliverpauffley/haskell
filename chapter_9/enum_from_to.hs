
eftBool :: Bool -> Bool -> [Bool]
eftBool True False  = []
eftBool True True   = [True]
eftBool False True  = [False, True]
eftBool False False = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start end
 |  start > end  = []
 | start == end =  [start]
 | otherwise = start : eftOrd (succ start) end


eftInt :: (Enum a, Ord a) => a-> a -> [a]
eftInt start end
        | start > end = []
        | start == end = [start]
        | otherwise = start : eftInt (succ start) end
