{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}

module MyLib where

import           Data.Char
import           Data.Monoid
import           Prelude     hiding (compare, lookup)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) rev cap

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = do
  capped <- cap
  reved <- rev
  return (capped, reved)

tupled''' :: [Char] -> ([Char], [Char])
tupled''' =
  cap . rev >>= (,)

newtype Reader r a
  = Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id

myLiftA2 ::
  (Applicative f) =>
  (a -> b -> c) ->
  f a ->
  f b ->
  f c
myLiftA2 fun a b = fun <$> a <*> b

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader a) = Reader (f . a)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) ::
    Reader r (a -> b) ->
    Reader r a ->
    Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure

  (>>=) ::
    Reader r a ->
    (a -> Reader r b) ->
    Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r

data Person = Person
  { dogName :: String,
    address :: String
  }
  deriving (Eq, Show)

data Dog = Dog
  { name        :: String,
    dogsAddress :: String
  }
  deriving (Eq, Show)

dogRm :: Reader Person Dog
dogRm = do
  n <- asks dogName
  a <- asks address
  return $ Dog n a

x = [1, 2, 3]

y = [4, 5, 6]

z = [7, 8, 9]

lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup key ((x,xy):xs)
  | key == x = Just xy
  | otherwise = lookup key xs

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n , z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt  = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $
    sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ (getAll . foldMap All . sequA) 4
