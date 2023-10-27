data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah


x = Blah
main :: IO ()
main = do
  print (changeMood x)
