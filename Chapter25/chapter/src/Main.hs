module Main where

import Data.Bifunctor

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose $ pure $ pure x
  -- fgf :: f (g (a -> b))
  -- fga :: f (g a))
  Compose fgf <*> Compose fga =
    Compose $ (fmap (<*>) fgf) <*> fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  -- f :: a -> f b
  -- sequenceA :: t (f a) -> f (t a)
  -- fga :: f (g a)
  -- return type :: f (Compose f1 g a)
  traverse f (Compose fga) = Compose <$> (sequenceA $ sequenceA <$> ((fmap . fmap) f fga))

data Deux a b = Deux a b deriving Eq

instance Bifunctor Deux where
  bimap f g (Deux x y) = Deux (f x) (g y)

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Deux a b) where
--   arbitrary = do
--     a <- arbitrary
--     Deux a <$> arbitrary

-- instance (Eq a, Eq b) => EqProp (Deux a b) where
--   (=-=) = eq

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const x) = Const $ f x

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei x y) = SuperDrei x (f y)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei x) = SemiDrei x

data Quadriceps a b c d = Quadriceps a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadriceps m n o p) = Quadriceps m n (f o) (g p)

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
  bimap f _ (Left' x) = Left' $ f x
  bimap _ g (Right' y) = Right' $ g y

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ f <$> fa

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  IdentityT fma <*> IdentityT fa = IdentityT $ fma <*> fa

instance Monad m => Monad (IdentityT m) where
  return = pure
  -- IdentityT ma >>= f = IdentityT $ (f <$> ma) >>= runIdentityT
  IdentityT ma >>= f = IdentityT $ ma >>= runIdentityT . f

main :: IO ()
main = do
  print "hello"
