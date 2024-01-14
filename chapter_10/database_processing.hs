module Database where

import           Data.Time
import           System.Posix (getUserEntryForID)

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

filterDates :: [DatabaseItem] -> [UTCTime]
filterDates = foldr onlyDates []
        where onlyDates (DbDate d) xs = d : xs
              onlyDates _ xs          = xs

filterNumbers :: [DatabaseItem] -> [Integer]
filterNumbers = foldr onlyNumbers []
        where onlyNumbers (DbNumber num) xs = num : xs
              onlyNumbers _ xs              = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum. filterDates


sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterNumbers

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral ys / fromIntegral  (length  numDb)
  where ys = sumDb xs
        numDb = filterNumbers xs
