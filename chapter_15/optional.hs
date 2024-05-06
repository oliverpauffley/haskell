data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) :: Semigroup a => Optional a -> Optional a -> Optional a
  (<>) (Only a) (Only b) = Only (a <> b)
  (<>) (Only a) Nada     =  Only a
  (<>) Nada (Only b)     =  Only b
  (<>) Nada Nada         = Nada


instance Monoid a
  => Monoid (Optional a) where
  mempty = Nada
