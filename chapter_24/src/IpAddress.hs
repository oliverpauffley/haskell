module IpAddress where

import           Data.Char               (isDigit)
import           Data.Word
import           Numeric                 (readHex)
import           Text.Parser.Char        (char, digit, oneOf)
import           Text.Parser.Combinators (eof, some)
import           Text.Trifecta           (Parser, Result (Success), hexadecimal,
                                          octal, parseString, (<?>))

newtype IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)


parseIPAddress :: Parser IPAddress
parseIPAddress = do
  octet1 <- parseOctet
  _ <- char '.'
  octet2 <- parseOctet
  _ <- char '.'
  octet3 <- parseOctet
  _ <- char '.'
  octet4 <- parseOctet
  return $ IPAddress (octetToDecimal octet1 octet2 octet3 octet4)

parseOctet :: Parser Int
parseOctet = read <$> some digit

octetToDecimal :: (Num a, Integral a) => a -> a -> a -> a -> Word32
octetToDecimal a b c d =
  fromIntegral (a * 256 ^ 3
  + b * 256 ^ 2
  + c * 256
  + d)

-- TODO fix this
data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord, Show)

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = do
  octet1 <- parseHexOctet <* char ':'
  octet2 <- parseHexOctet <* char ':'
  octet3 <- parseHexOctet <* char ':'
  octet4 <- parseHexOctet <* char ':'
  octet5 <- parseHexOctet <* char ':'
  octet6 <- parseHexOctet <* char ':'
  octet7 <- parseHexOctet <* char ':'
  octet8 <-  parseHexOctet <* eof
  let
    group1 = octet1 <> octet2 <> octet3 <> octet4
    group2 = octet5 <> octet6 <> octet7 <> octet8
  return $ IPAddress6 (hexToWord64 group1)  (hexToWord64 group2)


parseHexOctet :: Parser String
parseHexOctet = some $ oneOf (['0'..'9'] ++ ['A'..'F']) <?> "hexadecimal number"

hexToWord64 :: String -> Word64
hexToWord64 hex = case readHex hex of
  [(n, "")] -> n
  _         -> error "invalid hex digit"


test = parseString parseIPAddress mempty "172.16.254.1"

testIP6 = parseString parseIPAddress6 mempty "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
