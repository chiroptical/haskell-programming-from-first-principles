module Either' where

import Prelude hiding (Left, Right)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Data.Semigroup (Sum)

data Either' b a = Left a | Right b deriving (Eq, Show)

instance Functor (Either' b) where
    fmap _ (Right x) = Right x
    fmap f (Left y) = Left $ f y

instance Monoid b => Applicative (Either' b) where
    pure = Left
    Right x <*> Right x' = Right $ x <> x'
    Left f <*> Left y = Left $ f y
    _ <*> Right x = Right x
    Right x <*> _ = Right x
    
instance Monoid b => Monad (Either' b) where
    return = pure
    Right x >>= _ = Right x
    Left y >>= f = f y

instance (Arbitrary b, Arbitrary a) => Arbitrary (Either' b a) where
    arbitrary = oneof [ Left <$> arbitrary
                      , Right <$> arbitrary ]

instance (Eq b, Eq a) => EqProp (Either' b a) where
    (=-=) = eq

checkEither' :: Either' (Sum Int) (Int, String, Integer)
checkEither' = undefined