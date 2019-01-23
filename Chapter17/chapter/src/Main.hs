module Main where

import Data.List (elemIndex)

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Lookups
-- Use pure, <$>, or <*> to type check

-- zip [1, 2, 3] [4, 5, 6] = a = [(1, 4), (2, 5), (3, 6)]
-- lookup 3 a = Just 6
-- Just 9
added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

-- Just (6, 5)
tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

a :: Maybe Int
a = elemIndex 3 [1, 2, 3, 4, 5]

b :: Maybe Int
b = elemIndex 4 [1, 2, 3, 4, 5]

-- elemIndex 3 [1, 2, 3, 4, 5] == 2
-- liftA2 max (Just 2) (Just 3) == Just 3
maxed :: Maybe Int
maxed = max <$> a <*> b

xs = [1, 2, 3]
ys = [4, 5, 6]

m :: Maybe Integer
m = lookup 3 $ zip xs ys 

n :: Maybe Integer
n = lookup 2 $ zip xs ys

-- m = Just 6, n = Just 5
-- Just (6, 5)
summed :: Maybe Integer
summed = fmap sum $ (,) <$> m <*> n

newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant _ <*> Constant x = Constant x

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = Address <$> validateLength 100 s

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' -> Just $ Person n' a'

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a

-- Exercise: Fixer Upper, use <$>, <*> and pure to make the code work
-- 1.

one = const <$> Just "hello" <*> pure "world"

two = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = oneof [ return Fools
                    , return Twoo]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

instance EqProp Bull where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (monoid Twoo)