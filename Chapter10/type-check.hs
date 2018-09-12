-- foldl (flip (*)) 1 [1..3]
-- (((1 `f` 1) `f` 2) `f` 3)
-- ((1 `f` 2) `f` 3)
-- 2 `f` 3
-- 6

import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
    where f (DbDate x) a = [x] ++ a
          f _ a = a

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
    where f (DbNumber x) a = [x] ++ a
          f _ a = a

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)) . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = foldr (/) 1 [num, den]
    where num = fromIntegral . sumDb $ xs
          den = fromIntegral . length . filterDbNumber $ xs

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> [Integer]
fibsN n = take n fibs

fibsLtHundred :: [Integer]
fibsLtHundred = takeWhile (<100) fibs

fact :: [Integer]
fact = scanl (*) 1 [2..]

-- Chapter Exercises

stops = "pbtdkg"
vowels = "aeiou"

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

threes = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
threesBeginP = filter (\x -> fst3 x == 'p') threes

seekritFunc :: String -> Double
seekritFunc x = a / b
  where a = fromIntegral $ sum (map length (words x))
        b = fromIntegral $ length (words x)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a a'-> f a || a') False

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a as = a == foldr f h t
  where f x acc = if acc == a then a else x
        h = head as
        t = tail as

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (==a)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a l -> f a : l) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a l -> if f a then a : l else l) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a bs -> f a ++ bs) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Why not foldr?
myOrderingBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myOrderingBy o f [] = error "Cannot operate on empty list!"
myOrderingBy o f (a:[]) = a
myOrderingBy o f as = foldl (folder o f) (head as) as
--myOrderingBy o f as = foldl (\acc x -> if f acc x == o then acc else x) (head as) as

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myOrderingBy GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myOrderingBy LT

folder :: Ordering -> (a -> a -> Ordering) -> a -> a -> a
folder o f acc x = if f acc x == GT then acc else x
