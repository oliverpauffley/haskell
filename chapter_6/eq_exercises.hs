module Eqs where
import Data.List (insert)

newtype TisAnInteger = TisAn Integer
  deriving Show

instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'

data TwoIntegers = Two Integer Integer
  deriving Show

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt =
  TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False

data Pair a = Pair a a
  deriving Show

instance (Eq a) => Eq (Pair a) where
  (==) (Pair a a') (Pair b b') = a == b && a' == b'

data Tuple a b = Tuple a b
  deriving Show

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (/=) (Tuple x y) (Tuple a b)  = x /= a && y /= b

data Which a = ThisOne a | ThatOne a
  deriving Show

instance (Eq a) => Eq (Which a) where
  (==) :: Eq a => Which a -> Which a -> Bool
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) (ThisOne a) (ThatOne a') = a == a'
  (==) (ThatOne a) (ThisOne a') = a == a'

data EitherOr a b =
  Hello a
  | GoodBye b
  deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (GoodBye a) (GoodBye a') = a == a'
  (==) _ _ = False
