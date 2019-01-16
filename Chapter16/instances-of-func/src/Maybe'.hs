module Maybe' where

import Test.QuickCheck

data Maybe' a = Nothing' | Just' a deriving (Eq, Show)

instance Functor Maybe' where
    fmap _ Nothing' = Nothing'
    fmap f (Just' x) = Just' $ f x

instance Arbitrary a => Arbitrary (Maybe' a) where
    arbitrary = frequency [ (99, Just' <$> arbitrary)
                          , (1, return Nothing')]