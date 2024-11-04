module Main (main) where
import           MyLib                    (EitherT)
import           Test.QuickCheck.Checkers (quickBatch)
import           Test.QuickCheck.Classes  (monad)

main :: IO ()
main = quickBatch $ monad (undefined :: EitherT Integer [] (String, Int, Int))
