module Main where

import           Cipher             (uncaeser, unvigenere)
import qualified Cipher
import           System.Environment (getArgs)

dispatch :: [(String, String -> String -> String)]
dispatch = [ ("caeser", Cipher.caeser)
         , ("vigenere", Cipher.vigenere )
         , ("uncaeser", Cipher.uncaeser )
         , ("unvigenere", Cipher.unvigenere )
         ]

main :: IO ()
main = do
  (command:args) <- getArgs
  case lookup command dispatch of
    (Just action) ->
      putStrLn $ action (head args) $ concat args
    _ -> do
      putStrLn "usage: <cipher> shift text \
               \ where cipher is caeser/vigenere or uncaeser/unvigenere"
