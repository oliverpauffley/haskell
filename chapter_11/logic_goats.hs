{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (x, _) = tooMany x

instance TooMany (Int, Int) where
  tooMany (a, b) = tooMany (a + b)

instance (Num a, TooMany a) => TooMany (a, a)  where
  tooMany (a, b) = tooMany (a + b)
