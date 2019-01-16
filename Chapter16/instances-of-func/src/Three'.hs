module Three' where

import Test.QuickCheck

-- data Three a b c = Three a b c
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y y') = Three' x (f y) (f y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        Three' x y <$> arbitrary