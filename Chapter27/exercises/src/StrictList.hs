{-# LANGUAGE Strict #-}

module StrictList where

data List a = Nil | Cons a ~(List a) deriving Show

take' n _
  | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

map' _ Nil = Nil
map' f (Cons x xs) = Cons x (map' f xs)

repeat' x = 
  let xs = Cons x xs
   in xs
