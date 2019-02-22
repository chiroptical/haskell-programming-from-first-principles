module RedGreen where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- Either, Left == Red and Right == Green
data RedGreen a b = Red a | Green b deriving (Eq, Show)

instance Functor (RedGreen a) where
    fmap _ (Red x) = Red x
    fmap f (Green y) = Green (f y)

instance Applicative (RedGreen a) where
    pure = Green
    Red x <*> _ = Red x
    _ <*> Red x = Red x
    -- Green f <*> Green y = Green $ f y
    Green f <*> gy = fmap f gy

instance Foldable (RedGreen a) where
    foldMap _ (Red _) = mempty
    foldMap f (Green y) = f y

instance Traversable (RedGreen a) where
    traverse _ (Red x) = pure (Red x)
    traverse f (Green y) = Green <$> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (RedGreen a b) where
    arbitrary = oneof [ Red <$> arbitrary
                      , Green <$> arbitrary
                      ]

instance (Eq a, Eq b) => EqProp (RedGreen a b) where
    (=-=) = eq