module MyLib where

import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1' >> eof

one' = one >> stop

-- read two characters '1' and '2'
oneTwo = char '1' >> eof >> char '2' >> eof


-- read two characters then die
oneTwo' = oneTwo >> stop


p123 = string "123" >> eof

testParse :: Parser () -> IO ()
testParse p = print $ parseString p mempty "123"


charString = traverse char


c123 = charString "123" >> eof

pNL s =
  putStrLn ('\n' : s)


main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'

  pNL "string:"
  testParse p123

  pNL "charString:"
  testParse c123
