-- |

module Cipher where

import           Data.Char

-- converts a string into a caeser cipher shifted string
caesar :: Int -> String -> String
caesar shift = map (caesarLetter shift . toLower)

unCaesar shift = caesar (-shift)

minC = ord 'a'
maxC = ord 'z'

-- converts a single character into a caeser caesar shifted character
caesarLetter :: Int -> Char -> Char
caesarLetter shift c
  | ord c > minC && ord c< maxC = chr (wrapAddition (ord c) shift)
  | otherwise = c


wrapAddition :: Int -> Int -> Int
wrapAddition a b
  | y > maxC = minC + mod y maxC - 1
  | y < minC = maxC - (minC - y) + 1
  | otherwise = y
  where y = a + b
