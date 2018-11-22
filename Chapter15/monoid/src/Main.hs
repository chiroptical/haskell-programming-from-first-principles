{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

-- Magmas
-- x <> x'

-- associative rule
-- x <> (x' <> x'') == (x <> x') <> x''

-- left identity
-- x <> mempty == x

-- right identity
-- mempty <> x == x

-- If associative and identity laws: Monoid
-- If only associative: Semigroup

-- inverse
-- x <> inverse x == mempty

-- If associative, identity, and inverse: Group

-- commutative 
-- x <> x' == x' <> x

-- If associative, identity, inverse, and commutative: Abelian

data Bool' = False' | True' deriving (Eq, Show)

instance Semigroup Bool' where
  (<>) False' _ = False'
  (<>) _ False' = False'
  (<>) _ _ = True'

instance Monoid Bool' where
  mempty = True'

data Maybe' a = Nothing' | Just' a deriving (Eq, Show)

instance Semigroup a => Semigroup (Maybe' a) where
  (<>) Nothing' _ = Nothing'
  (<>) _ Nothing' = Nothing'
  (<>) (Just' x) (Just' x') = Just' $ x <> x'

instance Monoid a => Monoid (Maybe' a) where
  mempty = Just' mempty

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

leftIdent :: (Eq m, Monoid m) => m -> Bool
-- leftIdent a = mappend mempty a == a
leftIdent a = mempty <> a == a

rightIdent :: (Eq m, Monoid m) => m -> Bool
-- rightIdent a = mappend a mempty == a
rightIdent a = a <> mempty == a

class (Monoid m) => Group m where
  inverse :: m -> m

instance Group String where
  inverse = reverse

instance Num a => Group (Sum a) where
  inverse (Sum x) = Sum (negate x)

inverseProperty :: (Eq m, Group m) => m -> Bool
inverseProperty x = inverse x <> x == mempty

class (Group m) => Abelian m

instance Abelian String

instance Num a => Abelian (Sum a)

commutativeProperty :: (Eq a, Abelian a) => a -> a -> Bool
commutativeProperty x x' = x <> x' == x' <> x

newtype First' a = First' {getFirst' :: Maybe' a} deriving (Eq, Show)

newtype Last' a = Last' {getLast' :: Maybe' a} deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) f@(First' (Just' _)) _ = f
  (<>) _ f@(First' (Just' _)) = f
  (<>) _ _ = First' Nothing'

instance Semigroup (Last' a) where
  (<>) _ f@(Last' (Just' _)) = f
  (<>) f@(Last' (Just' _)) _ = f
  (<>) _ _ = Last' Nothing'

instance Monoid (First' a) where
  mempty = First' Nothing'

instance Monoid (Last' a) where
  mempty = Last' Nothing'

instance Arbitrary a => Arbitrary (Maybe' a) where
  arbitrary = do
    a <- arbitrary
    oneof [ return Nothing'
          , return $ Just' a]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary

data NonEmpty a = a :| [a] deriving Show

nonEmptyHead :: NonEmpty a -> a
nonEmptyHead (x :| _) = x

instance Semigroup (NonEmpty a) where
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ [y] ++ ys)

main :: IO ()
main = do
  putStrLn "hello"
  -- putStrLn "semigroupAssoc :: String -> String -> String -> Bool"
  -- quickCheck (semigroupAssoc :: String -> String -> String -> Bool)
  -- putStrLn "identities :: String -> Bool"
  -- quickCheck (leftIdent :: String -> Bool)
  -- quickCheck (rightIdent :: String -> Bool)
  -- putStrLn "inverse :: String -> Bool"
  -- quickCheck (inverseProperty :: String -> Bool)
  -- putStrLn "inverse :: Sum Int -> Bool"
  -- quickCheck (inverseProperty :: Sum Int -> Bool)
  -- putStrLn "commute :: String -> String -> Bool"
  -- quickCheck (commutativeProperty :: String -> String -> Bool)
  -- putStrLn "commute :: Sum Int -> Sum Int -> Bool"
  -- quickCheck (commutativeProperty :: Sum Int -> Sum Int -> Bool)
  -- quickCheck (semigroupAssoc :: First' String -> First' String -> First' String -> Bool)
  -- quickCheck (leftIdent :: First' String -> Bool)
  -- quickCheck (rightIdent :: First' String -> Bool)