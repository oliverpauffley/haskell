newtype Person = Person Bool
  deriving (Show)

printPerson :: Person -> IO ()
printPerson = print

data Mood = Blah | Woot
  deriving (Show, Eq)

settleDown x =
  if x == Woot
    then Blah
    else x

type Subject = String

type Verb = String

type Object = String

data Sentence
  = Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"

data Rocks
  = Rocks String
  deriving (Eq, Show)

data Yeah
  = Yeah Bool
  deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)


equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'


i :: Num a => a
i = 1

f :: Fractional a => a
f = 1.0
