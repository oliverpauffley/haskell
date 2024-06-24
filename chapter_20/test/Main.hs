module Main (main) where
import           Data.Monoid              (Any, Product, Sum)
import           MyLib
import           MyLib                    (Constant, Three, Two)
import           Test.Hspec
import           Test.QuickCheck          (verbose)
import           Test.QuickCheck.Checkers (quickBatch)
import           Test.QuickCheck.Classes  (foldable)

constant :: Constant (Sum Int) (Product Int,Sum Int, String,Int,Bool)
constant = undefined

two :: Two(Sum Int) (Product Int,Sum Int, String,Int,Bool)
two = undefined

three :: Three Bool Char (String, Int, Any,Int, Int)
three = undefined

three' :: Three' Bool (String, Int, Any,Int, Int)
three' = undefined

four :: Four Bool (String, Int, Any,Int, Int)
four = undefined

greaterThan3 :: Int -> Bool
greaterThan3 = (>3)

isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _   = False

main :: IO ()
main = do
  quickBatch $ foldable constant
  quickBatch $ foldable two
  quickBatch $ foldable three
  quickBatch $ foldable three'
  quickBatch $ foldable four
  hspec $ do
    describe "filterF" $ do
      it "filters numbers greater than 3" $ do
        (filterF greaterThan3 ([1,2,3,4])) `shouldBe` [4]
      it "filters vowels from words" $ do
        (filterF isVowel "hello") `shouldBe` "eo"
