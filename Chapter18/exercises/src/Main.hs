module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Monad (liftM2)

import Nope
import Either'
import Identity
import List

j :: Monad m => m (m a) -> m a
j mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = f <$> ma

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb =
  ma >>=
    \a -> mb >>=
      \b -> return $ f a b

l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f ma mb = do
  a <- ma
  f a <$> mb

l2'' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2'' = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a mx mf =
  mx >>= \x ->
    mf >>= \f ->
      return $ f x

a' :: Monad m => m a -> m (a -> b) -> m b
a' = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = f x >>=
  \b -> meh xs f >>=
    \bs -> return $ b : bs

meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' (x:xs) f = do
  b <- f x
  bs <- meh' xs f
  return $ b : bs

flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id

main :: IO ()
main = do
  -- quickBatch $ applicative checkNope
  -- quickBatch $ monad checkNope
  -- quickBatch $ applicative checkEither'
  -- quickBatch $ monad checkEither'
  -- quickBatch $ applicative checkIdentity
  -- quickBatch $ monad checkIdentity
  quickBatch $ applicative checkList
  quickBatch $ monad checkList