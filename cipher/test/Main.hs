module Main (main) where

import           Cipher
import           Data.Char                (toUpper)
import           Test.QuickCheck          (Gen, elements, listOf, quickCheck)
import           Test.QuickCheck.Property

caeserReversable :: Int -> String -> Bool
caeserReversable offset input =
  uncaeser (show offset) (caeser (show offset) input) == map toUpper input

prop_caeser_reversable :: Int -> Property
prop_caeser_reversable offset = forAll genSafeString (caeserReversable offset)

vigenereReversable :: String -> String ->  Bool
vigenereReversable offset input =
  unvigenere offset (vigenere offset input) == map toUpper input

prop_vigenere_reversable :: Property
prop_vigenere_reversable = forAll genSafeString (vigenereReversable "test")

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

main :: IO ()
main = do
  (quickCheck . verbose) prop_caeser_reversable
  (quickCheck . verbose) prop_vigenere_reversable
