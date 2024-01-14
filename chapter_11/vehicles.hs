import           GHC.Show (appPrec)
data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
             deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)

doge = Plane PapuAir (Size 10)

isCar :: Vehicle -> Bool
isCar (Plane _ _) = False
isCar (Car _ _)   = True

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane (Car _ _)   = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

