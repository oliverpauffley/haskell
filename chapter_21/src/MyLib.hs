{-# LANGUAGE InstanceSigs #-}
module MyLib where
import           Control.Applicative      (liftA3)
import           Data.Foldable            (Foldable (fold))
import           Debug.Trace              (trace)
import           Test.QuickCheck          (Arbitrary (arbitrary), Gen,
                                           Testable (property), frequency,
                                           sized, (.&&.))
import           Test.QuickCheck.Checkers (EqProp ((=-=)), eq)
import           Test.QuickCheck.Classes  (traversable)
import           Test.QuickCheck.Gen      (oneof)
import           Test.QuickCheck.Property


newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) =  Identity (f a)

instance Foldable Identity where
  foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse :: Applicative f => (a -> f b) -> Identity a -> f (Identity b)
  traverse fun (Identity a) = fmap Identity (fun a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (EqProp a) => EqProp (Identity a) where
  (=-=) (Identity a) (Identity b) = a =-= b

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant b) where
  fmap _ (Constant b) = Constant b

instance Foldable (Constant b) where
  foldMap :: Monoid m => (a -> m) -> Constant b a -> m
  foldMap _ _ = mempty

instance Traversable (Constant a) where
   traverse :: Applicative f => (a2 -> f b) -> Constant a1 a2 -> f (Constant a1 b)
   traverse _ (Constant a)  = pure (Constant a)

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (EqProp a) => EqProp (Constant a b) where
  (=-=) (Constant a) (Constant b) = a =-= b


data Optional a =
  Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap :: (a -> b) -> Optional a -> Optional b
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap :: Monoid m => (a -> m) -> Optional a -> m
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada),(5, Yep <$> arbitrary)]

instance (EqProp a) => EqProp (Optional a) where
  Nada =-= Nada   = property True
  Yep a =-= Yep b = a =-= b
  Nada =-= _      = property False
  _ =-= Nada      = property False

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil        = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Foldable List where
  foldMap :: Monoid m => (a -> m) -> List a -> m
  foldMap _ Nil        = mempty
  foldMap f (Cons a b) = f a <> foldMap f b

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse _ Nil        = pure Nil
  traverse f (Cons a b) = liftA2 Cons (f a) (traverse f b)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = sized genList

genList :: Arbitrary a => Int -> Gen (List a)
genList 0 = pure Nil
genList n = oneof [pure Nil, liftA2 Cons arbitrary (genList (n `div` 2))]

instance (EqProp a) => EqProp (List a) where
  (Cons a b) =-= (Cons c d) = (a =-= c) .&&. (b =-= d)
  Nil =-= Nil               = property True
  _ =-= _                   = property False


data Three a b c = Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap :: (a2 -> b2) -> Three a1 b1 a2 -> Three a1 b1 b2
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap :: Monoid m => (a2 -> m) -> Three a1 b a2 -> m
  foldMap f (Three _ _ c)= f c

instance Traversable (Three a b) where
  traverse :: Applicative f => (a2 -> f b2) -> Three a1 b1 a2 -> f (Three a1 b1 b2)
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (EqProp a, EqProp b, EqProp c) => EqProp (Three a b c) where
  (Three a b c) =-= (Three d e f) = a =-= d .&&. b =-= e .&&. c =-= f

data Pair a b = Pair a b
  deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap :: (a2 -> b) -> Pair a1 a2 -> Pair a1 b
  fmap f (Pair a b)= Pair a (f b)

instance Foldable (Pair a) where
  foldr :: (a2 -> b -> b) -> b -> Pair a1 a2 -> b
  foldr f i (Pair _ b)=  f b i

instance Traversable (Pair a) where
  sequenceA :: Applicative f => Pair a1 (f a2) -> f (Pair a1 a2)
  sequenceA (Pair a b) = Pair a <$> b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (EqProp a, EqProp b) => EqProp (Pair a b) where
  (Pair a b) =-= (Pair c d) = a =-= c .&&. b =-= d

data Big a b =  Big a b b
  deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap :: (a2 -> b) -> Big a1 a2 -> Big a1 b
  fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
  foldMap :: Monoid m => (a2 -> m) -> Big a1 a2 -> m
  foldMap f (Big _ b c) = f b <> f c

instance Traversable (Big a) where
  traverse :: Applicative f => (a2 -> f b) -> Big a1 a2 -> f (Big a1 b)
  traverse f (Big a b c)= Big a <$> f b <*> f c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = liftA3 Big arbitrary arbitrary arbitrary

instance (EqProp a, EqProp b) => EqProp (Big a b) where
  Big a b c =-= Big e f g = a =-= e .&&. b =-= f .&&. c =-= g

data Bigger a b =  Bigger a b b b
  deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap :: (a2 -> b) -> Bigger a1 a2 -> Bigger a1 b
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
  foldMap :: Monoid m => (a2 -> m) -> Bigger a1 a2 -> m
  foldMap f (Bigger _ b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where
  traverse :: Applicative f => (a2 -> f b) -> Bigger a1 a2 -> f (Bigger a1 b)
  traverse f (Bigger a b c d)= Bigger a <$> f b <*> f c <*> f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (EqProp a, EqProp b) => EqProp (Bigger a b) where
  Bigger a b c e =-= Bigger f g h i = a =-= f .&&. b =-= g .&&. c =-= h .&&. e =-= i

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap :: (a -> b) -> S n a -> S n b
  fmap f (S n a) = S (fmap f n) (f a)

instance (Foldable n) => Foldable (S n) where
  foldMap :: (Foldable n, Monoid m) => (a -> m) -> S n a -> m
  foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  traverse :: (Traversable n, Applicative f) => (a -> f b) -> S n a -> f (S n b)
  traverse f (S n a)= S <$> traverse f n <*> f a

instance ( Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a)
         , EqProp a) => EqProp (S n a) where
  (=-=) = eq


instance ( Functor n, Arbitrary (n a), Arbitrary a )
  => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty=       mempty
  foldMap f (Leaf a)     = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ i Empty        = i
  foldr f i (Leaf a)     = f a i
  foldr f i (Node l a r) =  left
    where right = foldr f i r
          middle = f a right
          left = foldr f middle l

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Empty        = pure Empty
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Node l a r) = liftA3 Node (traverse f l) (f a) (traverse f r)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = frequency [(3, return Empty), (2, Leaf <$> arbitrary), (1, liftA3 Node arbitrary arbitrary arbitrary)]


instance (EqProp a) => EqProp (Tree a) where
  Empty =-= Empty                     = property True
  Leaf a =-= Leaf b                   = a =-= b
  (Node la ma ra) =-= (Node lb mb rb) = la =-= lb .&&. ma =-= mb .&&. ra =-= rb
  _ =-= _                             = property False
