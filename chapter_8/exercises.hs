import           Data.List (intercalate, intersperse)

recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum 0 = 0
recursiveSum 1 = 1
recursiveSum n = n + recursiveSum (n - 1)

recursiveMulti :: (Integral a => a -> a -> a)
recursiveMulti 0 b = 0
recursiveMulti 1 b = b
recursiveMulti a b = b + recursiveMulti ( a - 1 ) b


data DividedResult = Result Integer | DividedByZero
  deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num 0 = DividedByZero
dividedBy num denom = go num denom 0 1
        where go n
                d count sign
                | n < 0 =  go (-n) d count (-sign)
                | d < 0 =  go n (-d) count (-sign)
                | n < d =  Result (sign * count)
                | otherwise =
                  go (n - d) d (count + 1) sign


mc91 :: (Ord a, Num a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 $ mc91 (n + 11)


digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise =  digits (div n 10) ++ [mod n 10]

intToWords :: Int -> [String]
intToWords n = map digitToWord $ digits n

wordNumber :: Int -> String
wordNumber n = intercalate "-" (intToWords n)


