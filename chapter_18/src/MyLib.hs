{-# LANGUAGE InstanceSigs #-}
module MyLib where
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp ((=-=)))

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First a)  _          = First a
  (<*>) (Second _) (First b)  = First b
  (<*>) (Second f) (Second b) = Second (f b)

instance Monad (Sum a) where
  return = pure
  (>>=) :: Sum a1 a2 -> (a2 -> Sum a1 b) -> Sum a1 b
  (Second a) >>= b =  b a
  (First a) >>= _  =  First a


data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap :: (a -> b) -> Nope a -> Nope b
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure :: a -> Nope a
  pure _ = NopeDotJpg
  (<*>) :: Nope (a -> b) -> Nope a -> Nope b
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  (>>=) :: Nope a -> (a -> Nope b) -> Nope b
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary :: Gen (Nope a)
  arbitrary = return NopeDotJpg

instance (Eq a) => EqProp (Nope a) where
  (=-=) _ _ = property True


data BahEither b a =
  PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap :: (a -> b2) -> BahEither b1 a -> BahEither b1 b2
  fmap f (PLeft a)  = PLeft (f a)
  fmap _ (PRight b) = PRight b

instance Applicative (BahEither b) where
  pure :: a -> BahEither b a
  pure = PLeft
  (<*>) :: BahEither b1 (a -> b2) -> BahEither b1 a -> BahEither b1 b2
  PLeft f <*> PLeft a = PLeft (f a)
  PRight a <*> _      = PRight a
  _ <*> PRight a      = PRight a

instance Monad (BahEither b) where
  (>>=) :: BahEither b1 a -> (a -> BahEither b1 b2) -> BahEither b1 b2
  (PLeft a) >>= f  =  f a
  (PRight b) >>= _ = PRight b

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither a b) where
  arbitrary = oneof [PLeft <$> arbitrary, PRight <$> arbitrary]

instance (EqProp a, EqProp b) => EqProp (BahEither a b) where
  PLeft a =-= PLeft b   = a =-= b
  PRight a =-= PRight b = a =-= b
  _ =-= _               = property False
