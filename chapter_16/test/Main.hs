module Main where

import           FunctorQuick
import           Test.QuickCheck



functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

c :: (Functor f, Eq (f Integer)) => f Integer -> Bool
c = functorCompose (+1) (*2)

identityIdent :: Identity Int -> Bool
identityIdent = functorIdentity

composeIdent :: Integer -> Bool
composeIdent x = c (Identity x)

identityPair :: Pair Int -> Bool
identityPair = functorIdentity

composePair :: Integer -> Integer -> Bool
composePair x y  = c (Pair x y)

identityTwo :: Two Int String -> Bool
identityTwo = functorIdentity

composeTwo :: String -> Integer -> Bool
composeTwo y x  = c (Two y x)

identityThree :: Three Int String Char -> Bool
identityThree = functorIdentity

composeThree :: Char -> String -> Integer -> Bool
composeThree x y z  = c (Three y x z)

identityThree' :: Three' Int String -> Bool
identityThree' = functorIdentity

composeThree' :: Char -> Integer -> Integer -> Bool
composeThree' x y z  = c (Three' x y z)

identityFour :: Four Int String Char Char -> Bool
identityFour = functorIdentity

composeFour :: Char -> String -> Bool -> Integer -> Bool
composeFour w x y z   = c (Four w y x z)

identityFour' :: Four' Int Char -> Bool
identityFour' = functorIdentity

composeFour' :: Char -> Char -> Char -> Integer -> Bool
composeFour' w x y z   = c (Four' w y x z)

main :: IO ()
main = do
  (quickCheck . verbose) identityIdent
  (quickCheck . verbose) composeIdent
  (quickCheck . verbose) identityPair
  (quickCheck . verbose) composePair
  (quickCheck . verbose) identityTwo
  (quickCheck . verbose) composeTwo
  (quickCheck . verbose) identityThree
  (quickCheck . verbose) composeThree
  (quickCheck . verbose) identityThree'
  (quickCheck . verbose) composeThree'
  (quickCheck . verbose) identityFour
  (quickCheck . verbose) composeFour
  (quickCheck . verbose) identityFour'
  (quickCheck . verbose) composeFour'
