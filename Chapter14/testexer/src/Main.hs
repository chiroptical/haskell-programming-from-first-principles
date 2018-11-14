module Main where

import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)

half :: Fractional a => a -> a
half = (/2)

prop_twoTimesHalfEqId :: Double -> Bool
prop_twoTimesHalfEqId x = ((*2) . half $ x) == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ s@(_, False) = s
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

-- foldr g acc [a, a', a'']
-- g a (g a' (g a'' acc))

-- [1, 2, 3]
-- go 1 (go 2 (go 3 (Nothing, True)))
-- go 1 (go 2 (Just 3, True))
-- go 1 (Just 2, True)
-- (Just 1, True)

-- foldl g acc [a, a', a'']
-- g (g (g acc a) a') a''

prop_intListOrdered :: [Int] -> Bool
prop_intListOrdered xs = listOrdered $ sort xs

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative = plusAssociative

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative = plusCommutative

multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x

prop_multAssociative :: Int -> Int -> Int -> Bool
prop_multAssociative = multAssociative

prop_multCommutative :: Int -> Int -> Bool
prop_multCommutative = multCommutative

-- (quot x y) * y + (rem x y) == x
quotRemTest :: (Eq a, Integral a) => a -> a -> Bool
quotRemTest x y = (quot x y) * y + (rem x y) == x

divModTest :: (Eq a, Integral a) => a -> a -> Bool
divModTest x y = (div x y) * y + (mod x y) == x

prop_quotRemTest :: Int -> NonZero Int -> Bool
prop_quotRemTest x (NonZero y) = quotRemTest x y

prop_divModTest :: Int -> NonZero Int -> Bool
prop_divModTest x (NonZero y) = divModTest x y

exponentiationAssoc :: (Eq a, Integral a) => a -> a -> a -> Bool
exponentiationAssoc x y z = x ^ (y ^ z) == (x ^ y) ^ z

exponentiationCommute :: (Eq a, Integral a) => a -> a -> Bool
exponentiationCommute x y = x ^ y == y ^ x

prop_exponentiationAssoc :: Int -> Int -> Int -> Bool
prop_exponentiationAssoc = exponentiationAssoc

prop_exponentiationCommute :: Int -> Int -> Bool
prop_exponentiationCommute = exponentiationCommute

revRevListIsId :: Eq a => [a] -> Bool
revRevListIsId xs = (reverse . reverse $ xs) == xs

prop_revRevListIsId :: [Int] -> Bool
prop_revRevListIsId = revRevListIsId

-- f $ a == f a
-- f . g == \x -> f (g x)

f :: Int -> Int
f = (*2)

g :: Int -> Int
g = (*4)

dollarTest :: Eq a => (a -> a) -> a -> Bool
dollarTest f a = (f $ a) == f a

compositionTest :: Eq a => (a -> a) -> (a -> a) -> a -> Bool
compositionTest f g a = (f . g) a == (\x -> f (g x)) a

prop_dollarTest :: Int -> Bool
prop_dollarTest = dollarTest f

prop_compositionTest :: Int -> Bool
prop_compositionTest = compositionTest f g

foldrEq :: (Eq a) => [a] -> [a] -> Bool
foldrEq xs ys = foldr (:) xs ys == (++) xs ys

foldrEq' :: (Eq a) => [[a]] -> Bool
foldrEq' xxs = foldr (++) [] xxs == concat xxs

prop_foldrEq :: [Int] -> [Int] -> Bool
prop_foldrEq = foldrEq

prop_foldrEq' :: [[Int]] -> Bool
prop_foldrEq' = prop_foldrEq'

-- f n xs = length (take n xs) == n
lenTakeTest :: Int -> [a] -> Bool
lenTakeTest n xs = length (take n xs) == n

prop_lenTakeTest :: NonNegative Int -> NonEmptyList Int -> Bool
prop_lenTakeTest (NonNegative n) (NonEmpty xs) = lenTakeTest n xs

-- f x = read (show x) == x
readShowTest :: (Show a, Read a, Eq a) => a -> Bool
readShowTest x = read (show x) == x

prop_readShowTest :: Int -> Bool
prop_readShowTest = readShowTest

square :: Floating a => a -> a
square x = x * x

squareIdentity :: Float -> Bool
squareIdentity x = (square . sqrt $ x) == x

prop_squareIdentity :: Float -> Bool
prop_squareIdentity = squareIdentity

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

capWordTest :: String -> Bool
capWordTest xs = capitalizeWord xs == twice capitalizeWord xs

capWordTest' :: String -> Bool
capWordTest' xs = capitalizeWord xs == fourTimes capitalizeWord xs

-- String = [Char]
prop_capWordTest :: NonEmptyList Char -> Bool
prop_capWordTest (NonEmpty xs) = capWordTest xs

prop_capWordTest' :: NonEmptyList Char -> Bool
prop_capWordTest' (NonEmpty xs) = capWordTest' xs

sortIdempotent :: (Ord a) => [a] -> Bool
sortIdempotent xs = sort xs == twice sort xs

sortIdempotent' :: (Ord a) => [a] -> Bool
sortIdempotent' xs = sort xs == fourTimes sort xs

prop_sortIdempotent :: [Int] -> Bool
prop_sortIdempotent = sortIdempotent

prop_sortIdempotent' :: [Int] -> Bool
prop_sortIdempotent' = sortIdempotent'

data Fool = Fulse | Frue deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary =
    frequency [ (1, return Fulse)
              , (1, return Frue)]

foolGen :: Gen Fool
foolGen = arbitrary

main :: IO ()
main = do
  -- quickCheck prop_twoTimesHalfEqId
  -- quickCheck prop_intListOrdered
  -- quickCheck prop_addAssociative
  -- quickCheck prop_addCommutative
  -- quickCheck prop_multAssociative
  -- quickCheck prop_multCommutative
  -- quickCheck prop_quotRemTest
  -- quickCheck prop_divModTest
  -- quickCheck prop_exponentiationAssoc
  -- quickCheck prop_exponentiationCommute
  -- quickCheck prop_revRevListIsId
  -- quickCheck prop_dollarTest
  -- quickCheck prop_compositionTest
  -- quickCheck prop_foldrEq
  -- quickCheck prop_foldrEq'
  -- quickCheck prop_lenTakeTest
  -- quickCheck prop_readShowTest
  -- quickCheck prop_squareIdentity
  -- quickCheck prop_capWordTest
  -- quickCheck prop_capWordTest'
  quickCheck prop_sortIdempotent
  quickCheck prop_sortIdempotent'