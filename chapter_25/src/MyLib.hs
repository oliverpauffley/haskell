{-# LANGUAGE InstanceSigs #-}
module MyLib  where

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g)
         => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose (pure (pure a))

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
        -- fmap to build an inner composeable f
  (Compose f) <*> (Compose a) =  Compose $ (fmap (<*>) f) <*> a

  liftA2 f (Compose fga) (Compose fgb) = Compose (liftA2 (liftA2 f) fga fgb)


instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: (Foldable f, Foldable g, Monoid m) =>
    (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
  traverse :: (Traversable f, Traversable g, Applicative f1) =>
    (a -> f1 b) -> Compose f g a -> f1 (Compose f g b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Duex a b = Duex a b
  deriving (Eq, Show)

instance Bifunctor Duex where
  first :: (a -> b) -> Duex a c -> Duex b c
  first f (Duex a b) = Duex (f a) b

  second :: (b -> c) -> Duex a b -> Duex a c
  second f (Duex a b) = Duex a (f b)

data Const a b = Const a

instance Bifunctor Const where
  first :: (a -> b) -> Const a c -> Const b c
  first f (Const a) = Const (f a)
  second :: (b -> c) -> Const a b -> Const a c
  second _ (Const a) = Const a


data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  first :: (a2 -> b) -> Drei a1 a2 c -> Drei a1 b c
  first f (Drei a b c) = Drei a (f b) c
  second :: (b -> c) -> Drei a1 a2 b -> Drei a1 a2 c
  second f (Drei a b c) = Drei a b (f c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  first f (SuperDrei a b) = SuperDrei a (f b)
  second _ (SuperDrei a b) = SuperDrei a b

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap :: (a2 -> b) -> (c -> d) -> SemiDrei a1 a2 c -> SemiDrei a1 b d
  bimap _ _ (SemiDrei c)= SemiDrei c

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  first :: (a2 -> b2) -> Quadriceps a1 b1 a2 c -> Quadriceps a1 b1 b2 c
  first f (Quadzzz a b c d) = Quadzzz a b (f c) d
  second :: (b2 -> c) -> Quadriceps a1 b1 a2 b2 -> Quadriceps a1 b1 a2 c
  second f (Quadzzz a b c d) = Quadzzz a b c (f d)

data MyEither a b = MLeft a | MRight b

instance Bifunctor (MyEither) where
  bimap :: (a -> b) -> (c -> d) -> MyEither a c -> MyEither b d
  bimap f _ ( MLeft a)  = MLeft (f a)
  bimap _ g ( MRight b) = MRight (g b)
