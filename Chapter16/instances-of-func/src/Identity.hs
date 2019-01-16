module Identity where

import Test.QuickCheck

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

-- instance Show a => Show (Identity a) where
--     show (Identity x) = "Identity " ++ show x

-- instance Eq a => Eq (Identity a) where
--     (==) (Identity x) (Identity x') = x == x'