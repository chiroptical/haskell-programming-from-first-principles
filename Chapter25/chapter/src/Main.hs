module Main where

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
  -- fga :: f (g a)
  -- return type :: f (Compose f1 g a)
  traverse f (Compose fga) = _help

main :: IO ()
main = do
  putStrLn "hello world"
