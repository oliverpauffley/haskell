module Main where

import           Data.Char  (isAsciiUpper, toLower)
import           Data.List  (elemIndex, foldl')
import           Data.Maybe (fromMaybe)
type Presses = Int

data Digit = One | Two| Three | Four | Five | Six | Seven | Eight | Nine | Star | Zero | Hash
  deriving (Eq, Show)

newtype KeyPad = KeyPad [(Digit , String)]
 deriving (Show)

defaultPhone = KeyPad [
  (One, "1")
  ,(Two, "abc2")
  ,(Three, "def3")
  ,(Four, "ghi4")
  ,(Five, "jkl5")
  ,(Six, "mno6")
  ,(Seven, "pqrs7")
  ,(Eight, "tuv8")
  ,(Nine, "wxyz9")
  ,(Star, "^*")
  ,(Zero, "+ 0")
  ,(Hash, ".,#")
  ]


convo =["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol OK. Have u ever tasted alcohol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "OK. Do u think I am pretty Lol",
        "Lol ya",
        "Just making sure rofl ur turn"]


-- assuming a default phone
-- 'a' -> [(Two, 1)]
-- 'A' -> [(Star, 1), (Two, 1)]
reverseTaps :: KeyPad -> Char -> [(Digit, Presses)]
reverseTaps pad c
   | isAsciiUpper c = (Star, 1) : reverseTaps pad (toLower c)
   | otherwise  = [reverseTap pad c]


reverseTap :: KeyPad -> Char -> (Digit, Presses)
reverseTap (KeyPad []) c = (Zero, 0)
reverseTap (KeyPad ((d, pad):ps)) c = if c `elem` pad
  then (d, fromMaybe 0 (c `elemIndex` pad) + 1)
  else reverseTap (KeyPad ps) c

messageToPresses :: KeyPad -> String -> [(Digit, Presses)]
messageToPresses = undefined


convertConversation :: [String] -> [(Digit,Presses)]
convertConversation = foldl (\a b -> a ++ convertWord b) []

convertWord :: String -> [(Digit, Presses)]
convertWord = foldl (\a b-> a ++ reverseTaps defaultPhone b) []


fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldl' (\sum (_, p) -> sum + p) 0

main :: IO ()
main = do
 print defaultPhone
