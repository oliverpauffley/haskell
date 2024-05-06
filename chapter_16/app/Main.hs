module Main where

replaceWithP :: b -> Char
replaceWithP = const 'p'

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = fmap replaceWithP

twiceLifted :: (Functor f1, Functor f) =>
                f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = (fmap . fmap) replaceWithP

thriceLifted :: (Functor f2, Functor f1, Functor f)
  => f(f1 (f2 a)) -> f(f1(f2 Char))
thriceLifted = (fmap . fmap . fmap ) replaceWithP

thriceLifted' :: [Maybe [Char]]-> [Maybe [Char]]
thriceLifted' = (fmap . fmap . fmap ) replaceWithP

main :: IO ()
main = do
  print (replaceWithP' lms)
  print (liftedReplace' lms)
  print (twiceLifted lms)
  print (thriceLifted lms)
