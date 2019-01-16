{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck

-- data Bool = False | True                     # no Functor possible, Bool is kind *
-- data BoolAndSomething a = False a | True a   # Functor possible, BoolAndSomething is kind * -> *
-- data BoolAndMaybe a = False | True a         # Functor possible, BoolAndMaybe is kind * -> * 
-- newtype Mu f = InF { outF :: f (Mu f) }      # Functor possible, Mu f is kind * -> *

-- data C a = C (Array Word Word) a a           # Functor possible, C is kind * -> *
-- data D = D (Array Word Word) Int Int         # no Functor possible, D is kind *

data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company a b) where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Floor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f (Floor y) = Floor (f y)

instance Functor (Flip Quant b) where
  fmap _ (Flip Finance) = Flip Finance
  fmap f (Flip (Desk x)) = Flip $ Desk (f x)
  fmap _ (Flip (Floor y)) = Flip $ Floor y

data K a b = K a

instance Functor (K a) where
  fmap _ (K x) = K x

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K y)) = Flip $ K (f y)

data EvilGoateeConst a b = GoateeConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoateeConst x) = GoateeConst (f x)

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fx) = LiftItOut (fmap f fx)

data Parappa f g a = Parappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (Parappa fx gx) = Parappa (fmap f fx) (fmap f gx)

data IgnoreOne f g a b = IgnoreOne (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap g (IgnoreOne fx gy) = IgnoreOne fx (fmap g gy)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap g (Notorious go ga gt) = Notorious go ga (fmap g gt)

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y) 

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat 
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats y y' y'') = MoreGoats (fmap f y) (fmap f y') (fmap f y'')

main :: IO ()
main = do
  putStrLn "hello world"
