module Main where

import Test.QuickCheck

data FixMePls a =
  FixMe | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap f (Pls x) = Pls (f x)
  fmap _ FixMe = FixMe

-- fmap id == id
identityLaw :: (Functor f, Eq (f a)) => f a -> Bool
identityLaw f = fmap id f == id f

-- fmap (f . g) == fmap f . fmap g
compositionLaw :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
compositionLaw f g x = fmap (g . f) x == (fmap g . fmap f) x

instance Show (a -> b) where
  show _ = "_"

data WhoCares a =
  ItDoesnt | Matter a | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter x) = Matter (f x)

instance Arbitrary a => Arbitrary (WhoCares a) where
  arbitrary = frequency [ (1, pure ItDoesnt)
                         ,(1, Matter <$> arbitrary)
                         ,(1, pure WhatThisIsCalled)
                        ] 

data CountingBad a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n x) = Heisenberg n (f x)

instance Arbitrary a => Arbitrary (CountingBad a) where
  arbitrary = do
    i <- arbitrary
    Heisenberg i <$> arbitrary
  
a :: [Int]
a = (+1) <$> read "[1]" :: [Int]

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Int -> Int
c = (*2) . (\x -> x - 2)

d :: Int -> String
d = ((return '1' ++) . show) . (\a -> [a, (a + 1) .. (a + 3)])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        change = fmap (read . ("123" ++) . show) ioi
    in fmap (*3) change

data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Functor (Or a) where
  fmap f (Second x) = Second (f x)
  fmap _ (First x) = First x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    Two x <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [ First <$> arbitrary
                    , Second <$> arbitrary ] 

main :: IO ()
main = do
  quickCheck (identityLaw :: Maybe Bool -> Bool)
  quickCheck (compositionLaw :: (Int -> String) -> (String -> Char) -> Maybe Int -> Bool)
  quickCheck (identityLaw :: WhoCares Int -> Bool)
  quickCheck (compositionLaw :: (Int -> String) -> (String -> Char) -> WhoCares Int -> Bool)
  quickCheck (identityLaw :: CountingBad Int -> Bool)
  quickCheck (compositionLaw :: (Int -> String) -> (String -> Char) -> CountingBad Int -> Bool)
  quickCheck (identityLaw :: Two Int Double -> Bool)
  quickCheck (compositionLaw :: (Int -> Integer) -> (Integer -> Double) -> Two Bool Int -> Bool)
  quickCheck (identityLaw :: Or Int Double -> Bool)
  quickCheck (compositionLaw :: (Int -> Integer) -> (Integer -> Double) -> Or Bool Int -> Bool)