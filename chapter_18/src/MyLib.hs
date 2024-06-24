{-# LANGUAGE InstanceSigs #-}
module MyLib where
import           Control.Monad            (join)
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp ((=-=)), eq)

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


data Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity
  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  Identity a >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (EqProp a) => EqProp (Identity a) where
  Identity a =-= Identity b = a =-= b

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  Nil <> x               = x
  x <> Nil               = x
  Cons x xs <> Cons y ys = Cons x (xs <> Cons y ys)

instance Monoid (List a) where
  mempty :: List a
  mempty = Nil

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil        = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure :: a -> List a
  pure a = Cons a Nil
  (<*>) _ Nil  = Nil
  (<*>)  Nil _ = Nil
  (<*>) rs ss  = flatMap (flip fmap ss) rs

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b ) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap _ Nil = Nil
flatMap f l   = concat' (fmap f l)

instance Monad List where
  Nil >>= _      = Nil
  Cons x t >>= f = f x <> (t >>= f)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    do
      a <- arbitrary
      frequency [( 1, return (Cons a Nil)) ,( 1, return Nil )]


instance (Eq a) => EqProp (List a) where
  (=-=) = eq


j :: Monad m => m (m a) -> m a
j a = a >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f a = f <$> a

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*>  b

a :: Monad m => m a -> m (a -> b) -> m b
a xs f = f <*> xs

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _     = return []
meh (x:xs) f = fmap (:) (f x)  <*>  meh xs f

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
