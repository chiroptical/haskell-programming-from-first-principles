module Optional where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Optional a = Nada | Yup a deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yup x) = Yup $ f x

instance Applicative Optional where
    pure = Yup
    _ <*> Nada = Nada
    Nada <*> _ = Nada
    Yup f <*> xs = fmap f xs

instance Foldable Optional where
    foldMap f (Yup x) = f x
    foldMap _ _ = mempty

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yup x) = Yup <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = oneof [ pure Nada
                      , Yup <$> arbitrary
                      ]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq