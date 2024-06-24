module Main (main) where
import           MyLib                    (BahEither, Identity, List, Nope)
import           Test.QuickCheck.Checkers (quickBatch)
import           Test.QuickCheck.Classes  (applicative, functor, monad)

nope :: Nope (Int, String, Int)
nope = undefined

bahEither :: BahEither Int (Int, String, Int)
bahEither = undefined

identity :: Identity (Int, String, Int)
identity = undefined

list :: List (Int, String, Int)
list = undefined

main :: IO ()
main = do
  quickBatch $ functor nope
  quickBatch $ applicative nope
  quickBatch $ monad nope
  quickBatch $ functor bahEither
  quickBatch $ applicative  bahEither
  quickBatch $ monad  bahEither
  quickBatch $ functor identity
  quickBatch $ applicative  identity
  quickBatch $ monad  identity
  quickBatch $ functor list
  quickBatch $ applicative list
  quickBatch $ monad list
