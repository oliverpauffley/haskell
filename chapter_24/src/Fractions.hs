{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fractions where

import           Control.Applicative
import           Data.ByteString              (ByteString)
import           Data.Fixed                   (Fixed (MkFixed))
import           Data.Maybe                   (fromMaybe)
import           Data.Ratio                   ((%))
import           Data.Scientific              (Scientific)
import           Data.Time                    (TimeOfDay (TimeOfDay, todHour, todMin),
                                               midnight, pastMidnight, todSec)
import           Test.QuickCheck              (Arbitrary (arbitrary), Gen,
                                               Property, choose, chooseAny,
                                               elements, frequency, listOf,
                                               listOf1, oneof, sized, vectorOf,
                                               (===))
import           Test.QuickCheck.Gen          (suchThat)
import           Test.QuickCheck.Modifiers    (PrintableString (getPrintableString))
import           Text.Parser.LookAhead
import           Text.ParserCombinators.ReadP (skipSpaces)
import           Text.RawString.QQ            (r)
import           Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"


parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

type DecimalOrFraction =
  Either Scientific Rational

parseDecimal :: Parser Scientific
parseDecimal = scientific

parseDoF:: Parser DecimalOrFraction
parseDoF = (Left <$> try parseDecimal)
  <|> (Right <$> try virtuousFraction)


virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

type NumberOrString =
  Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
  (Left <$> integer)
  <|> (Right <$> some letter)

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = (char '-' *> (negate <$> base10Integer)) <|> base10Integer


-- Phone number parsing
type AreaCode = String
type SubscriberNumber = String

data PhoneNumber = PhoneNumber AreaCode SubscriberNumber
  deriving (Eq, Show)

exampleNum = "01202 841844"
exampleNum'= "01224 841844"
expectedNum = PhoneNumber "01202" "841844"

parsePhone :: Parser PhoneNumber
parsePhone = do
  area <- count 5 parseDigit
  skipMany space
  sub <- count 6 parseDigit
  return $ PhoneNumber area sub
-- TODO finish this if I can be bothered

-- log file format parser

type Year = Integer
type Month = Integer
type Day = Integer

type Name = String

instance Arbitrary TimeOfDay where
    arbitrary = TimeOfDay
        <$> choose (0, 23)
        <*> choose (0, 59)
        <*> return (MkFixed 00)

type Start = TimeOfDay

type End =  Maybe TimeOfDay
type Duration = Integer

data Date = Date Year Month Day
  deriving (Eq, Ord)

instance Show Date where
  show :: Date -> String
  show (Date y m d) =
    show y ++ "-" ++ show m ++ "-" ++ show d

genDay :: Gen Integer
genDay = choose (1,31)

genMonth :: Gen Integer
genMonth = choose (1,12)

genYear :: Gen Integer
genYear = choose (2000,2256)

instance Arbitrary Date where
  arbitrary = liftA3 Date genYear genMonth genDay

data Activity = Activity Name Start End
  deriving (Show, Ord)

instance Eq Activity where
  (Activity n1 s1 _) == (Activity n2 s2 _) = n1 == n2 && s1 == s2

genName :: Gen String
genName = listOf1 $ elements (['a'..'z'] ++ ['.', ','])

instance Arbitrary Activity where
  arbitrary = liftA3 Activity genName arbitrary arbitrary

genActivities :: Gen [Activity]
genActivities = sized genSizedActivities

genSizedActivities :: Int -> Gen [Activity]
genSizedActivities n = do
  x <- arbitrary
  xs <- frequency[(1, pure []), (n `div` 4, genSizedActivities n)]
  return (x:xs)

newtype ActivityLog = ActivityLog [(Date ,[Activity])]
  deriving (Eq, Show)

instance Arbitrary ActivityLog where
  arbitrary = ActivityLog <$> sized genSizedActivityLog

genSizedActivityLog :: Int -> Gen [(Date, [Activity])]
genSizedActivityLog n = do
  d <- liftA2 (,) arbitrary genActivities
  ds  <- frequency [(1, pure []), (n `div` 4, genSizedActivityLog n)]
  return (d:ds)

printActivityLog :: ActivityLog -> String
printActivityLog (ActivityLog []) = ""
printActivityLog (ActivityLog xs) = unlines $ toStrings xs
  where toStrings :: [(Date, [Activity])] -> [String]
        toStrings [] = []
        toStrings ((d, l): ds) =
          let
            dte = "#" ++ show d
            lgs = printActivities l
          in
            dte : lgs ++ toStrings ds

printActivities :: [Activity] -> [String]
printActivities [] = []
printActivities ((Activity name (TimeOfDay h m _)_):xs) =
  (show h ++ ":" ++ show m ++ " " ++ name ): printActivities xs

newtype DurationLog = DurationLog [(Date, [(Name, Duration)])]
  deriving (Eq, Show)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany (do
               _ <- parseComment
               skipMany (noneOf "\n")
               skipEOL)

parseComment :: Parser String
parseComment = count 2 (char '-')

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseDate :: Parser Date
parseDate = do
  _  <- char '#'
  skipMany space
  year <- integer
  _  <- char '-'
  month <- integer
  _  <- char '-'
  day <- integer
  _ <- many newline
  return $ Date year month day

parseActivity :: Parser Activity
parseActivity = do
  skipWhitespace
  skipComments
  start <- parseTime
  skipWhitespace
  actvity <- manyTill anyChar (try $ lookAhead parseComment <|> string "\n")
  skipWhitespace
  skipComments
  end <- optional (lookAhead parseTime)
  return $ Activity actvity start end

parseActivities :: Parser [Activity]
parseActivities = manyTill parseActivity eof

diffTimeOfDay :: TimeOfDay -> TimeOfDay -> Integer
diffTimeOfDay x y =  abs minDiff + abs hourDiff
  where hourDiff = toInteger (todHour x - todHour y) * 60
        minDiff = toInteger $ todMin x - todMin y


parseTime :: Parser TimeOfDay
parseTime = do
  hour <- fromInteger <$> integer
  _ <- char ':'
  minutes <- fromInteger <$> integer
  return $ TimeOfDay hour minutes 0

parseDay :: Parser (Date, [Activity])
parseDay = do
  date <- parseDate
  activities <- many parseActivity
  skipWhitespace
  skipComments
  return (date, activities)

parseLog :: Parser ActivityLog
parseLog = do
  days <- many parseDay
  eof
  return $ ActivityLog days


summarizeDay :: [Activity] -> [(Name, Duration)]
summarizeDay [] = []
summarizeDay (x:xs) = summarize x : summarizeDay xs
  where summarize :: Activity -> (Name, Duration)
        summarize (Activity name start end) =
          (name, diffTimeOfDay (fromMaybe midnight end) start)

buildDurationLog :: [(Date, [Activity])]-> [(Date, [(Name, Duration)])]
buildDurationLog []                     = []
buildDurationLog ((date, activities):xs) = (date, summarizeDay activities): buildDurationLog xs

activityLogToDurationLog :: ActivityLog -> DurationLog
activityLogToDurationLog (ActivityLog xs) = DurationLog $ buildDurationLog xs

exampleLog :: String
exampleLog = [r|# 2025-02-05 -- with comment
08:00 Breakfast -- and another comment
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Exercising in low-grav gym
|]

exampleActivity :: String
exampleActivity = "08:00 Breakfast\n"

exampleActivities :: String
exampleActivities = [r|12:00 fighting
13:15 running
|]

exampleActivities' :: String
exampleActivities' = [r|12:00 fighting -- gotta do those fights!
13:15 running
|]

exampleLog' :: ActivityLog
exampleLog'  = unwrapped result
  where result = parseString parseLog mempty exampleLog
        unwrapped (Success got) = got
        unwrapped _             = error "could not parse"



prop_parse_print :: ActivityLog -> Property
prop_parse_print l = do
  unwrapResult (parseString parseLog mempty (printActivityLog l)) === l

unwrapResult :: Result a -> a
unwrapResult (Success val) = val
unwrapResult _             = error "could not unwrap"


main :: IO()
main = do
  let parseFraction' =
        parseString virtuousFraction mempty

      p f i = parseString f mempty i

  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork

  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

  print $ p (some letter) a
  print $ p integer b

  print $ p parseNos a
  print $ p parseNos b

  print $ p (many parseNos) c
  print $ p (some parseNos) c
