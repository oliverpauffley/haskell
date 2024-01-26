data Nat = Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ a) = 1 + natToInteger a

integerToNat :: Integer -> Maybe Nat
integerToNat i = case compare 0 i of
  GT -> Nothing
  EQ -> Just Zero
  LT -> Just (builder i)
  where builder 0 = Zero
        builder i = Succ (builder (i-1))
