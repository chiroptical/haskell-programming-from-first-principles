addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

functionC :: Ord a => a -> a -> a
functionC x y = case x > y of
    True -> x
    False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = case even n of
    True -> n + 2
    False -> n

nums :: (Num a, Ord a) => a -> String
nums x = case compare x 0 of
    LT -> "negative!"
    GT -> "positive!"
    EQ -> "zero!"

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1

oneIsTwo :: (Num a) => a -> a
oneIsTwo = (flip dodgy) 2

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = div x 10
          d = mod xLast 10

divModWithTen :: (Integral a) => a -> (a, a)
divModWithTen = (flip divMod) 10

tensDigit' :: Integral a => a -> a
tensDigit' x = tens
     where allTens = fst (divModWithTen x)
           tens = snd (divModWithTen allTens)

hunsDigit :: Integral a => a -> a
hunsDigit x = huns
    where allHuns = fst (divModWithTen x)
          huns = fst (divModWithTen allHuns)

foldBoolPM :: a -> a -> Bool -> a
foldBoolPM x _ False = x
foldBoolPM _ y True = y

foldBoolITE :: a -> a -> Bool -> a
foldBoolITE x y tf = if tf then y else x

foldBoolC :: a -> a -> Bool -> a
foldBoolC x y tf = case tf of
    True -> y
    False -> x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
 
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show
