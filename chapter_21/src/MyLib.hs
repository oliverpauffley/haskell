{-# LANGUAGE InstanceSigs #-}
module MyLib where
import           Test.QuickCheck          (Arbitrary (arbitrary), Gen,
                                           Testable (property), frequency,
                                           sized, (.&&.))
import           Test.QuickCheck.Checkers (EqProp ((=-=)))
import           Test.QuickCheck.Gen      (oneof)


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
