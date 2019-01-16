module Main where

import Test.QuickCheck

import Identity as I
import Pair as P
import Two
import Three'
import Maybe'
import Either'

-- data Trivial = Trivial

instance Show (a -> b) where
  show _ = "_"

-- fmap id == id
identityLaw :: (Functor f, Eq (f a)) => f a -> Bool
identityLaw f = fmap id f == id f

-- fmap (f . g) == fmap f . fmap g
compositionLaw :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
compositionLaw f g x = fmap (g . f) x == (fmap g . fmap f) x

main :: IO ()
main = do
  quickCheck (identityLaw :: I.Identity Int -> Bool)
  quickCheck (compositionLaw :: (Double -> [Int]) -> ([Int] -> Int) -> I.Identity Double -> Bool)
  quickCheck (identityLaw :: P.Pair Int -> Bool)
  quickCheck (compositionLaw :: (Double -> [Int]) -> ([Int] -> Int) -> P.Pair Double -> Bool)
  quickCheck (identityLaw :: Two Double Int -> Bool)
  quickCheck (compositionLaw :: (Double -> [Int]) -> ([Int] -> Int) -> Two Int Double -> Bool)
  quickCheck (identityLaw :: Three' Double Int -> Bool)
  quickCheck (compositionLaw :: (Double -> [Int]) -> ([Int] -> Int) -> Three' Int Double -> Bool)
  quickCheck (identityLaw :: Maybe' Int -> Bool)
  quickCheck (compositionLaw :: (Double -> [Int]) -> ([Int] -> Int) -> Maybe' Double -> Bool)
  quickCheck (identityLaw :: Either' Int Double -> Bool)
  quickCheck (compositionLaw :: (Double -> [Int]) -> ([Int] -> Int) -> Either' Int Double -> Bool)