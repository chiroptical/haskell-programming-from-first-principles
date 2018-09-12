{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)


myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 50)

isCar :: Vehicle -> Bool
isCar (Car m p) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane a s) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m p) = m
getManu _ = error "Vehicle isn't a Car!"

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Integer where
    tooMany n = n > 42

newtype Goats = Goats Integer deriving (Eq, Show, TooMany)

-- GeneralizedNewtypeDeriving Pragma at top of file
-- instance TooMany Goats where
--    tooMany (Goats n) = tooMany n

-- FlexibleInstances Pragma at top of file
instance TooMany (Integer, String) where
    tooMany (x, _) = tooMany x

-- Another option without FlexibleInstances
-- newtype IString = IString (Integer, String) deriving (Eq, Show)
-- instance TooMany IString where
--     tooMany (IString (x, _)) = tooMany x

instance TooMany (Integer, Integer) where
    tooMany (x, x') = tooMany $ x + x'

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, x') = tooMany $ x + x'
