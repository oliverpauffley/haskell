{-# LANGUAGE InstanceSigs #-}
module MyLib  where
import           Test.QuickCheck (Arbitrary (arbitrary))

someFunc :: IO ()
someFunc = putStrLn "someFunc"


sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr equals False
  where equals b v = v || (x ==b)

minimum :: (Foldable t, Ord a)
        =>  t a -> Maybe a
minimum = foldr comp Nothing
  where comp a (Just b)
          | a < b  = Just a
          | otherwise  = Just b
        comp a Nothing = Just a

maximum :: (Foldable t, Ord a)
        =>  t a -> Maybe a
maximum = foldr comp Nothing
  where comp a (Just b)
          | a > b  = Just a
          | otherwise  = Just b
        comp a Nothing = Just a


null :: (Foldable t) => t a -> Bool
null =  foldr checker True
  where checker _ _ = False

length :: (Foldable t) => t a -> Int
length = foldr counter 0
  where counter _ sum = sum + 1

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- | Combine the elements
-- of a structure using a monoid
fold :: ( Foldable t, Monoid m) => t m -> m
fold = foldMap id


folderMap :: (Foldable t, Monoid m)
        => (a -> m) -> t a -> m
folderMap f = foldr (\c b -> f c <> b) mempty

data Constant a b =
  Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f i (Constant b) = f b i

instance Arbitrary b => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary



data Two a b = Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f i (Two _ b) = f b i

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b


data Three a b c = Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap :: Monoid m => (a2 -> m) -> Three a1 b a2 -> m
  foldMap f (Three _ _ c) =  f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap :: Monoid m => (a2 -> m) -> Three' a1 a2 -> m
  foldMap f (Three' _ b c) =  f b <> f c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

data Four a b = Four a b b b
  deriving (Eq, Show)

instance Foldable (Four a) where
  foldMap :: Monoid m => (a2 -> m) -> Four a1 a2 -> m
  foldMap f (Four _ b c d) =  f b <> f c <> f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d


-- [a] [a]
filterF :: (Applicative f, Foldable t, Monoid (f a))=> (a -> Bool) -> t a -> f a
filterF f = foldMap (filt f)
  where filt func a
          | func a = pure a
          | otherwise = mempty
