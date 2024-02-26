import           System.Directory.Internal.Prelude (stdout)
import           System.IO                         (BufferMode (NoBuffering),
                                                    hSetBuffering)

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson ::
  Name ->
  Age ->
  Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $
        PersonInvalidUnknown $
          "Name was: "
            ++ show name
            ++ " Age was: "
            ++ show age

gimmePerson :: IO ()
gimmePerson = do
        hSetBuffering stdout NoBuffering
        putStr "Enter a name: "
        nameInput <- getLine
        putStr "Enter an age (>0): "
        ageInput <- getLine
        case mkPerson nameInput (read ageInput) of
                Left err -> putStrLn $ "could not make a person " ++ show err
                Right person -> putStrLn $ "Yay! Successfully got a person: " ++ show person
