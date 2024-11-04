{-# LANGUAGE InstanceSigs #-}
module MyLib where

import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.DList                as DL

newtype Moi s a
  = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi
      ( \s ->
          let (result, s') = g s
           in (f result, s')
      )

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  (<*>) ::
    Moi s (a -> b) ->
    Moi s a ->
    Moi s b
  (Moi f) <*> (Moi g) =
    Moi
      ( \s ->
          let (func, s') = f s
              (a, s'') = g s'
           in (func a, s'')
      )

instance Monad (Moi s) where
  return = pure

  (>>=) ::
    Moi s a ->
    (a -> Moi s b) ->
    Moi s b
  (Moi f) >>= g =
    Moi
      ( \s ->
          let (a, s') = f s
              (b, s'') = runMoi (g a) s'
           in (b, s'')
      )


fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list =
  execState (mapM_ addResultD list) DL.empty


addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

addResultD :: Integer -> State (DL.DList String) ()
addResultD n = do
  xs <- get
  let result = fizzBuzz n
  -- snoc appends to the end, unlike cons which adds to the front
  put (DL.snoc xs result)

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to = execState (mapM_ addResult list) []
  where list = [to,to - 1..from]

main :: IO()
main =
  mapM_ putStrLn $ fizzBuzzFromTo 1 100

get' :: Moi s s
get' = Moi (\s -> (s, s))

put' :: s -> Moi s ()
put' newState = Moi (const ((), newState))

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

modify :: (s -> s) -> Moi s ()
modify f =  Moi (\s -> ((), f s))
