import           Data.Char

capitaliseWord :: String -> String
capitaliseWord []       = []
capitaliseWord (' ':xs) = ' ': capitaliseWord xs
capitaliseWord (x:xs)   = toUpper x : xs

capitaliseParagraphs :: String -> String
capitaliseParagraphs xs = concatMap capitaliseWord (splitOn '.' xs)

splitOn :: Char -> String -> [String]
splitOn d = foldr f [[]]
  where f c xs@(x:xt)
          | c == d    = [c]:xs
          | otherwise = (c:x):xt
