module MyLib where

import           Test.QuickCheck hiding (Failure, Success)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary =
    do
      ar <- arbitrary
      return (Identity ar)

data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two f g) = Two (x <> f) (y <> g)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary =
    do
      ar <- arbitrary
      br <- arbitrary
      return (Two ar br)

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three f g h) = Three (x <> f) (y <> g) (z <> h)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary =
    do
      ar <- arbitrary
      br <- arbitrary
      cr <- arbitrary
      return (Three ar br cr)

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True   = BoolConj True
  BoolConj True <> BoolConj False  = BoolConj False
  BoolConj False <> BoolConj True  = BoolConj False
  BoolConj False <> BoolConj False = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary =
    do
      b <- arbitrary
      return (BoolConj b)

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd a) <> _       = Snd a
  (Fst _) <> (Snd b) = Snd b
  (Fst a) <> (Fst _) = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary =
    do
      a <- arbitrary
      b <- arbitrary
      oneof [return (Fst a), return (Snd b)]

newtype Combine a b
  = Combine {unCombine :: a -> b}

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
  (Success x) <> _           = Success x
  _ <> (Success x)           = Success x
  (Failure a) <> (Failure b) = Failure (a <> b)

newtype Mem s a
  = Mem
  { runMem :: s -> (a, s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem (\x -> let (f1, x') = f x
                                      (g1, x'') = g x'
                                  in (f1 <> g1, x'')
                           )

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))
