{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Marshalling where

import           Control.Applicative
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Scientific      (floatingOrInteger)
import qualified Data.Text            as T
import           Data.Vector
import           Text.RawString.QQ

sectionJson :: ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
  }
  |]


data TestData =
  TestData {
        section :: Host
        , what  :: Color
        } deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) =
    TestData <$> v .: "section"
             <*> v .: "whatisit"
  parseJSON _ = fail "Expected an object for TestData"

newtype Host = Host String deriving (Eq, Show)

instance FromJSON Host where
  parseJSON (Object v) =
    Host <$> v .: "host"
  parseJSON _ =
    fail "Expected an object for Host"

type Annotation = String

data Color = Red Annotation | Blue Annotation | Yellow Annotation
  deriving (Eq, Show)


instance FromJSON Color where
  parseJSON (Object v) =
    (Red <$> v.: "red")
    <|> (Blue <$> v.: "blue")
    <|> (Yellow <$> v.: "hello")
  parseJSON _ =
    fail "expected an object for Color"

data NumberOrString =
  Numba Integer
  | Stringy T.Text
  deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of
      (Left _) ->
        fail "Must be integral number"
      (Right integer) ->
        return $ Numba integer
  parseJSON (String s) = return $ Stringy s
  parseJSON _ =
    fail "NumberOrString must\
         \ be number or string"

-- so it knows what we want to parse
dec :: ByteString -> Maybe (Vector NumberOrString)
dec = decode

eitherDec :: ByteString -> Either String (Vector NumberOrString)
eitherDec = eitherDecode

main = do
  let d :: Maybe TestData
      d = decode sectionJson
  print d
  print $ eitherDec "blah"
