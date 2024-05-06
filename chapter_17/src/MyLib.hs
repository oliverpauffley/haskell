module MyLib where

import           Control.Applicative      (liftA3)
import           Data.Monoid              (Sum)
import           Test.QuickCheck          (Arbitrary (arbitrary), property)
import           Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import           Test.QuickCheck.Classes  (applicative, monoid)
import           Test.QuickCheck.Gen

-- 1
-- added :: Maybe Integer
-- added = (+3) <$> lookup 3 (zip [1,2,3] [4,5,6])

-- -- 2
-- y :: Maybe Integer
-- y = lookup 3 $ zip [1,2,3] [4,5,6]

-- z :: Maybe Integer
-- z = lookup 2 $ zip [1,2,3] [4,5,6]

-- tupled :: Maybe (Integer, Integer)
-- tupled =  (,) <$> y <*> z


-- -- 3
-- x' :: Maybe Int
-- x' = elemIndex 3 [1,2,3,4,5]

-- y' :: Maybe Int
-- y' = elemIndex 4 [1,2,3,4,5]

-- max' :: Int -> Int -> Int
-- max' = max

-- maxed :: Maybe Int
-- maxed =  max' <$> x' <*> y'

-- -- 4
-- xs = [1,2,3]
-- ys = [4,5,6]

-- x'' :: Maybe Integer
-- x'' = lookup 3 $ zip xs ys

-- y'' :: Maybe Integer
-- y'' = lookup 2 $ zip xs ys

-- summed :: Maybe Integer
-- summed = sum <$> liftA2 (,) x'' y''

-- -- Applicative Identity

-- newtype Identity a = Identity a
--   deriving (Eq, Ord, Show)

-- instance Functor Identity where
--   fmap f (Identity a) = Identity (f a)

-- instance Applicative Identity where
--   pure = Identity
--   (Identity f) <*> (Identity a) = Identity (f a)


-- newtype Constant a b =
--   Constant { getConstant :: a }
--   deriving (Eq, Ord, Show)

-- instance Functor (Constant a) where
--   fmap _ (Constant a ) = Constant a

-- instance Monoid a => Applicative (Constant a) where
--   pure _ = Constant mempty
--   (Constant a) <*> (Constant b) = Constant (mappend a b)


-- -- Fixer Upper

-- fixer = const <$> Just "Hello" <*> Just "World"

-- upper = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]


-- -- Applicative testing
data Bull =
  Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [(1, return Fools)
              , (1, return Twoo)]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

instance EqProp Bull where
  (=-=) = eq



data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Applicative List where
  pure v = Cons v Nil
  (<*>) _ Nil  = Nil
  (<*>)  Nil _ = Nil
  (<*>) rs ss  = flatMap (flip fmap ss) rs

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    do
      a <- arbitrary
      frequency [( 1, return (Cons a Nil)) ,( 1, return Nil )]


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

newtype Ziplist' a =
  Ziplist' [a]
  deriving (Eq, Show)

instance Eq a => EqProp (Ziplist' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (Ziplist' l) = xs
                    in take 3000 l
          ys' = let (Ziplist' l) = ys
                    in take 3000 l

instance Functor Ziplist' where
  fmap f (Ziplist' xs) =
    Ziplist' $ fmap f xs

instance Applicative Ziplist' where
  pure a = Ziplist' (repeat a)
  (<*>) (Ziplist' fs)  (Ziplist' xs) = Ziplist' (go fs xs)
    where
      go (g:gs) (y:ys) = g y : go gs ys
      go _ []          = []
      go [] _          = []


instance (Arbitrary a) => Arbitrary (Ziplist' a) where
  arbitrary = Ziplist' <$> arbitrary

data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure a) = Failure a
  fmap f (Success a) = Success ( f a)


instance Monoid e => Applicative (Validation e) where
  pure = Success
  Failure a  <*> Failure b    = Failure (a <> b)
  (Failure a) <*> _           = Failure a
  _ <*> (Failure a)           = Failure a
  (Success f) <*> (Success a) = Success (f a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  Failure a =-= Failure b = a `eq` b
  Success a =-= Success b = a `eq` b
  _ =-= _                 = property False

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (Pair f g) <*> (Pair a b) = Pair( f a ) (g b )

instance (Eq a) => EqProp (Pair a) where
  (Pair a b) =-= (Pair d e) = if (a == d) && (b == e) then property True else property False

instance (Arbitrary a) => Arbitrary (Pair a) where
 arbitrary = liftA2 Pair arbitrary arbitrary

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure = Two mempty
  (Two f g) <*> (Two a b) = Two (f <> a) (g b)

instance (Eq a, Eq b) => EqProp (Two a b) where
  Two a b =-= Two d e = if (a == d) && (b == e) then property True else property False
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary


data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three f g h) <*> (Three a b c) = Three (f <> a) (g <> b) (h c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  Three a b c =-= Three d e f = if (a == d) && (b == e) && (c == f) then property True else property False

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' f g h) <*> (Three' a b c) = Three' (f <> a) (g b) (h c)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  Three' a b c =-= Three' d e f = if (a == d) && (b == e) && (c == f) then property True else property False

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary


stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

main :: IO ()
main = do
  quickBatch (monoid Twoo)
  quickBatch (applicative (Cons (1 :: Integer,"2", '3') Nil))
  quickBatch (applicative (undefined :: Ziplist' (String, Char, Integer)))
  quickBatch (applicative (undefined :: Validation [String] (String, Char, Integer)))
  quickBatch (applicative (undefined :: Pair (String,Char, Integer)))
  quickBatch (applicative (undefined :: Two [String] (String,Char, Integer)))
  quickBatch (applicative (undefined :: Three [String] (Sum Int) (String,Char, Integer)))
