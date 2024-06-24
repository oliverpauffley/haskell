module Main (main) where
import           MyLib                    (Constant, Identity, List, Optional)
import           Test.QuickCheck.Checkers (quickBatch)
import           Test.QuickCheck.Classes  (traversable)



main :: IO ()
main = do
  let trigger :: Identity ([Int], [Int], [Int], [Int])
      trigger = undefined
  let triggeb :: Constant Int ([Int], [Int], [Int], [Int])
      triggeb = undefined
  let triggec :: Optional ([Int], [Int], [Int], [Int])
      triggec = undefined
  let trigged :: List ([Int], [Int], [Int], [Int])
      trigged = undefined
  quickBatch $ traversable trigger
  quickBatch $ traversable triggeb
  quickBatch $ traversable triggec
  quickBatch $ traversable trigged
