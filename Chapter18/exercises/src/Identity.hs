module Identity where

import Test.QuickCheck
import Test.QuickCheck.Checkers

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
    Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

checkIdentity :: Identity (Int, String, Integer)
checkIdentity = undefined