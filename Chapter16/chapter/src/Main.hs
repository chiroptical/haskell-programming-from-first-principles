{-# LANGUAGE RankNTypes #-}

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
  -- arbitrary = choose (First <$> arbitrary, Second <$> arbitrary)
  -- arbitrary = oneof [ First <$> arbitrary
  --                   , Second <$> arbitrary ]
  arbitrary = frequency [ (1, First  <$> arbitrary)
                        , (1, Second <$> arbitrary)]

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just x) = Just $ x + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just x) = Just (show x)
showIfJust Nothing = Nothing

incIfJust' :: Num a => Maybe a -> Maybe a
incIfJust' = fmap (+1)

showIfJust' :: Show a => Maybe a -> Maybe String
showIfJust' = fmap show

liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

constantIdentityLaw :: Constant Int Int -> Bool
constantIdentityLaw xc@(Constant _) = getConstant (fmap id xc) == getConstant (id xc)

constantCompositionLaw :: (Int -> Int) -> (Int -> Int) -> Constant Int Int -> Bool
constantCompositionLaw f g xc@(Constant _) = getConstant (fmap (f . g) xc) == getConstant (fmap f . fmap g $ xc)

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

getInt :: IO Int
getInt = fmap read getLine

meToo :: IO String
meToo = do
  input <- getLine
  return (input ++ " and me too!")

meToo' :: IO String
meToo' = (++ " and me too!") <$> getLine

bumpIt :: IO Int
bumpIt = (+1) <$> getInt

-- fmap :: (a -> b) -> f a -> f b
-- nat :: (f -> g) -> f a -> g a

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- This is not a natural transformation!
-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing = []
-- degenerateMtl (Just x) = [x + 1]

main :: IO ()
main = do
  -- 16.0 -> 16.9
  -- quickCheck (identityLaw :: Maybe Bool -> Bool)
  -- quickCheck (compositionLaw :: (Int -> String) -> (String -> Char) -> Maybe Int -> Bool)
  -- quickCheck (identityLaw :: WhoCares Int -> Bool)
  -- quickCheck (compositionLaw :: (Int -> String) -> (String -> Char) -> WhoCares Int -> Bool)
  -- quickCheck (identityLaw :: CountingBad Int -> Bool)
  -- quickCheck (compositionLaw :: (Int -> String) -> (String -> Char) -> CountingBad Int -> Bool)
  -- quickCheck (identityLaw :: Two Int Double -> Bool)
  -- quickCheck (compositionLaw :: (Int -> Integer) -> (Integer -> Double) -> Two Bool Int -> Bool)
  -- quickCheck (identityLaw :: Or Int Double -> Bool)
  -- quickCheck (compositionLaw :: (Int -> Integer) -> (Integer -> Double) -> Or Bool Int -> Bool)
  
  -- 16.11 -> *
  quickCheck constantIdentityLaw
  quickCheck constantCompositionLaw