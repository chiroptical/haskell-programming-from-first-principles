module Either' where

import Test.QuickCheck

data Either' a b = Left' a | Right' b deriving (Eq, Show)

instance Functor (Either' a) where
    fmap f (Right' x) = Right' $ f x
    fmap _ (Left' x) = Left' x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either' a b) where
    arbitrary = oneof [ Left'  <$> arbitrary
                      , Right' <$> arbitrary]