module Main (main) where
import           MyLib                    (BahEither, Nope)
import           Test.QuickCheck.Checkers (quickBatch)
import           Test.QuickCheck.Classes  (applicative, functor, monad)

nope :: Nope (Int, String, Int)
nope = undefined

bahEither :: BahEither Int (Int, String, Int)
bahEither = undefined


main :: IO ()
main = do
  quickBatch $ functor nope
  quickBatch $ applicative nope
  quickBatch $ monad nope
  quickBatch $ functor bahEither
  quickBatch $ applicative  bahEither
  quickBatch $ monad  bahEither
