module Main where
import           Data.Maybe (isJust, isNothing)

replaceThe :: String -> String
replaceThe = unwords . replace . words
  where
    replace [] = []
    replace (x:xs) = case notThe x of
          Just y  -> y : replace xs
          Nothing -> "a" : replace xs

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a     = Just a

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = go 0 $ words xs

go :: Integer -> [String] -> Integer
go a [x, y]
  | isNothing (notThe x) && isVowel (head y) = a+1
  | otherwise = a
go a (x:y:xs)
  | isNothing (notThe x) && isVowel (head y) = go (a+1) (y:xs)
  | otherwise = go a (y:xs)
go a _ = a



countOnFilter :: (a -> Bool) -> [a]-> Int
countOnFilter f = length . filter f

countVowels :: String -> Int
countVowels = countOnFilter isVowel

countConsonants :: String -> Int
countConsonants = countOnFilter (not . isVowel)

newtype Word' = Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord input = if countConsonants input < countVowels input then Nothing else Just (Word' input)

main = do
  putStrLn $ replaceThe "the dog jumped over the fence"
