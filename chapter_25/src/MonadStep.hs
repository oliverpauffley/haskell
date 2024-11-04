{-# LANGUAGE InstanceSigs #-}
module MonadStep where
import           Control.Monad         (join)
import           Data.Functor.Identity (Identity (runIdentity))

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor f) => Functor (IdentityT f) where
  fmap :: Functor f => (a -> b) -> IdentityT f a -> IdentityT f b
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative f) => Applicative (IdentityT f) where
  pure :: Applicative f => a -> IdentityT f a
  pure a = IdentityT (pure a)

  (<*>) :: Applicative f =>
    IdentityT f (a -> b) -> IdentityT f a -> IdentityT f b
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: Monad m => IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
