module Main where

-- To view generated Core
-- :set -ddump-simpl
-- :set -dsuppress-all

noChainReaction :: Int
noChainReaction =
  let x = undefined
      y = 2
      z = (x `seq` y `seq` 10, 11)
   in snd z

-- Evaluates to first data constructor and no further
dc = (,) undefined undefined
noDc = undefined
lam = \_ -> undefined

data Test = A Test2 | B Test2 deriving Show
data Test2 = C Int | D Int deriving Show

forceNothing :: Test -> Int
forceNothing _ = 0

forceTest :: Test -> Int
forceTest (A _) = 1
forceTest (B _) = 2

forceTest2 :: Test -> Int
forceTest2 (A (C _)) = 1
forceTest2 (A (D _)) = 2
forceTest2 (B (C _)) = 3
forceTest2 (B (D _)) = 4

-- Exercises Evaluate
-- const 1 undefined
-- const :: a -> b -> a
-- (\a -> \b -> a) 1
-- \b -> 1 
-- 1 

-- const undefined 1 --> undefined

-- flip const undefined 1 --> 1

-- flip const 1 undefined --> undefined

-- const undefined undefined --> undefined

-- foldr const 'z' ['a'..'e'] --> 'a'

-- foldr (flip const) 'z' ['a'..'e'] --> 'z'

main :: IO ()
main = do
  -- print $ dc `seq` 1
  -- print $ lam `seq` 1
  -- print $ noDc `seq` 1

  print "hello"
