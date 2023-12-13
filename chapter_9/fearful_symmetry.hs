import           Data.Char (isSpace)

myWords :: String -> [String]
myWords []         = []
myWords (' ' : xs) = myWords xs
myWords xs         = takeWhile (/= ' ') xs : myWords (dropWhile (/= ' ') xs)

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines []          = []
myLines ('\n' : xs) = myLines xs
myLines xs          = takeWhile (/= '\n') xs : myLines (dropWhile (/= '\n') xs)

mySpliter :: Char -> String -> [String]
mySpliter _ [] = []
mySpliter c (x : xs)
  | c == x = mySpliter c xs
  | otherwise = takeWhile (/= c) (x : xs) : mySpliter c (dropWhile (/= c) (x : xs))
