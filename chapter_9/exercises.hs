module Chars where

import           Data.Char


filterUpper = filter isUpper

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

shout :: String -> String
shout []     = []
shout (x:xs) = toUpper x : shout xs
-- shout a      = map toUpper a

shoutFirst :: String -> Char
shoutFirst = toUpper . head
