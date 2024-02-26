module Cipher where

import           Data.Char

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift op offset ch = numToChar $ charToNum (toUpper ch) `op` offset
  where
    numToChar n = chr $ (n `mod` 26) + ord 'A'

vigenere :: String -> String -> String
vigenere secret = zipWith (shift (+)) (cycle $ map charToNum secret) . concat . words

unvigenere :: String -> String -> String
unvigenere secret = zipWith (shift (-)) (cycle $ map charToNum secret) . concat . words

caeser :: String -> String -> String
caeser offset = map (shift (+) (read offset))

uncaeser :: String -> String -> String
uncaeser offset = map (shift (-) (read offset))

charToNum :: Char -> Int
charToNum ch = ord ch - ord 'A'
