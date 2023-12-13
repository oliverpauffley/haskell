-- these are the the same!
mTH x y z = x * y * z
mTB x y = \z -> x * y * z
mTC x = \y -> \z -> x * y * z
mTD  = \x -> \y -> \z -> x * y * z


addOneIfOdd n = case odd n of
  True  -> \n -> n + 1
  False -> n

addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (if x >y then y else x) + 5

mFlip f x y = f y x

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User =
  UnregisterUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisterUser =
  putStrLn "UnregisterUser"

printUser (RegisteredUser
            (Username name )
            (AccountNumber acctNum)) =
  putStrLn $ name ++ " " ++ show acctNum
