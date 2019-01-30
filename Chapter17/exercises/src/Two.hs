module Two where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    Two x fy <*> Two x' y = Two (x <> x') (fy y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        Two a <$> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq