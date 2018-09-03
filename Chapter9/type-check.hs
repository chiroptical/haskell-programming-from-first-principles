import Data.Char (toUpper, toLower)
import Data.Bool (bool)

-- Originally wrote this, but saw the recursive pattern
-- eftBool :: Bool -> Bool -> [Bool]
-- eftBool b e
--     | b < e = [b, e]
--     | b == e = [b]
--     | otherwise = []

eftBool :: Bool -> Bool -> [Bool]
eftBool b e
    | b < e = [b] ++ eftBool (succ b) e
    | b == e = [b]
    | otherwise = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd b e
    | b < e = [b] ++ eftOrd (succ b) e
    | b == e = [b]
    | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt b e
    | b < e = [b] ++ eftInt (succ b) e
    | b == e = [b]
    | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar b e
    | b < e = [b] ++ eftChar (succ b) e
    | b == e = [b]
    | otherwise = []

enumFromTo' :: (Enum a, Ord a) => a -> a -> [a]
enumFromTo' b e
    | b < e = [b] ++ enumFromTo' (succ b) e
    | b == e = [b]
    | otherwise = []

-- myWords :: String -> [String]
-- myWords n
--     | takeWhile (/=' ') n == n = [n]
--     | otherwise = [takeWhile (/=' ') n] ++ myWords (tail . dropWhile (/=' ') $ n)

-- Recognize Abstraction
splitOn' :: Char -> String -> [String]
splitOn' sep n
    | takeWhile (/=sep) n == n = [n]
    | otherwise = [takeWhile (/=sep) n] ++ splitOn' sep (tail . dropWhile (/=sep) $ n)

myWords :: String -> [String]
myWords = splitOn' ' '

myLines :: String -> [String]
myLines = splitOn' '\n'

acronym :: String -> String
acronym xs = map toUpper [head x | x <- words xs]

vowels :: String -> String
vowels xs = [x | x <- map toLower xs, elem x "aeiou"]

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

myTups = [(x, y) | x <- mySqr, y <- myCube]
myTups' = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
-- myTups' = [(x, y) | (x, y) <- myTups, x < 50, y < 50] ?
myTups'' = length myTups'

-- Check if each char in string is a vowel
itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

negateN :: Int -> [Int] -> [Int]
negateN _ [] = []
negateN n (x:xs) = bool x (-x) (x == n) : negateN n xs

negateN' :: Int -> [Int] -> [Int]
negateN' _ [] = []
negateN' n xs = map (\x -> bool x (-x) (x == n)) xs

filterArticles :: String -> [String]
filterArticles = filter (\x -> not . elem x $ ["the", "a", "an"]) . words

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = [(a, b)] ++ zip' as bs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (a:as) (b:bs) = [f a b] ++ zipWith' f as bs

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (\x y -> (x, y))

capFirst :: String -> String
capFirst [] = []
capFirst (x:xs) = toUpper x : xs

capitalize :: String -> String
capitalize [] = []
capitalize (x: xs) = toUpper x : capitalize xs

capOnlyFirst :: String -> Char
capOnlyFirst = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr . map f $ xs

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = myAny (==x) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [[]] = []
squish [(x:xs)] = [x] ++ squish [xs]
squish ((x:xs):ys) = [x] ++ squish [xs] ++ squish ys

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain [[]] = []
squishAgain [(xs)] = squishMap (\x -> [x]) xs
squishAgain ((xs):ys) = squishMap (\x -> [x]) xs ++ squishAgain ys

myOrderingBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myOrderingBy o f [] = error "can't give empty list!"
myOrderingBy o f (x:[]) = x
myOrderingBy o f (x:y:[])
    | f x y == o = x
    | otherwise = y
myOrderingBy o f (x:y:ys)
    | f x y == o = myOrderingBy o f (x:ys)
    | otherwise = myOrderingBy o f (y:ys)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myOrderingBy GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myOrderingBy LT

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare
