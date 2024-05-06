module Main (main) where
import           Data.Monoid
import           MyLib
import           Test.QuickCheck



semigroupAssoc :: (Eq m, Semigroup m)
                => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b<>c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity m =
  mempty <> m == m

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentity m =
  m <> mempty == m

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type Ident = Identity (Sum Int)

type IdentAssoc = Ident -> Ident -> Ident -> Bool

type TwoIntString = Two (Sum Int) String

type TwoAssoc = TwoIntString -> TwoIntString -> TwoIntString -> Bool

type ThreeIntStringBool = Three (Sum Int) String Any

type ThreeAssoc = ThreeIntStringBool -> ThreeIntStringBool -> ThreeIntStringBool -> Bool

type BoolConjAssoc =  BoolConj -> BoolConj -> BoolConj -> Bool

type OrStringInt = Or String Int

type OrStringIntAssoc =  OrStringInt -> OrStringInt -> OrStringInt -> Bool

main :: IO ()
main =
  do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    quickCheck (semigroupAssoc :: IdentAssoc)
    quickCheck (monoidLeftIdentity :: Identity (Sum Int) -> Bool)
    quickCheck (monoidRightIdentity :: Identity (Sum Int) -> Bool)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: Two String (Sum Int) -> Bool)
    quickCheck (monoidRightIdentity :: Two String (Sum Int) -> Bool)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    quickCheck (semigroupAssoc :: OrStringIntAssoc)
