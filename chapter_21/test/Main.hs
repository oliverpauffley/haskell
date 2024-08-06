module Main (main) where
import           Data.Monoid              (Sum)
import           Data.Semigroup           (Product)
import           MyLib                    (Big, Constant, Identity, List,
                                           Optional, Pair, S, Three, Tree)
import           Test.QuickCheck.Checkers (quickBatch)
import           Test.QuickCheck.Classes  (traversable)


type TraverseType = (Product Int, Maybe Int, Int, Sum Int)

main :: IO ()
main = do
  let trigger :: Identity TraverseType
      trigger = undefined
  let triggeb :: Constant Int TraverseType
      triggeb = undefined
  let triggec :: Optional TraverseType
      triggec = undefined
  let trigged :: List TraverseType
      trigged = undefined
  let triggee :: Three Int Int TraverseType
      triggee = undefined
  let triggef :: Pair Int TraverseType
      triggef = undefined
  let triggeg :: Big Int TraverseType
      triggeg = undefined
  let triggeh :: S (Maybe)  TraverseType
      triggeh = undefined
  let triggei :: Tree TraverseType
      triggei = undefined
  quickBatch $ traversable trigger
  quickBatch $ traversable triggeb
  quickBatch $ traversable triggec
  quickBatch $ traversable trigged
  quickBatch $ traversable triggee
  quickBatch $ traversable triggef
  quickBatch $ traversable triggeg
  quickBatch $ traversable triggeh
  quickBatch $ traversable triggei
