{-# LANGUAGE InstanceSigs #-}

module MyLib where

newtype Moi s a
  = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi
      ( \s ->
          let (result, s') = g s
           in (f result, s')
      )

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  (<*>) ::
    Moi s (a -> b) ->
    Moi s a ->
    Moi s b
  (Moi f) <*> (Moi g) =
    Moi
      ( \s ->
          let (func, s') = f s
              (a, s'') = g s'
           in (func a, s'')
      )

instance Monad (Moi s) where
  return = pure

  (>>=) ::
    Moi s a ->
    (a -> Moi s b) ->
    Moi s b
  (Moi f) >>= g =
    Moi
      ( \s ->
          let (a, s') = f s
              (b, s'') = runMoi (g a) s'
           in (b, s'')
      )
