module Main where

import           Criterion.Main
import           DList
import           Queue

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n - 1) ([n] ++ xs)

constructDList :: Int -> [Int]
constructDList i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) (singleton n `append` xs)

q :: Queue Integer
q = Queue [1 .. 500] [1 .. 500]

queueViaQueue :: Queue Integer -> Queue Integer
queueViaQueue qi = go 10000 qi
  where
    go 0 q = q
    go n q
      | n `mod` 2 == 0 = push 0 q
      | otherwise =
        let Just (_, q') = pop q
         in q'

q' :: [Integer]
q' = [1 .. 1000]

queueViaList :: [Integer] -> [Integer]
queueViaList qi = go 10000 qi
  where
    go 0 q = q
    go n q
      | n `mod` 2 == 0 = 0 : q
      | otherwise = init q

main :: IO ()
main =
  defaultMain
  -- [ bench "concat list" $
  --   whnf schlemiel 123456
  -- , bench "concat DList" $
  --   whnf constructDList 123456
    [ bench "queue list" $ nf queueViaList q'
    , bench "queue Queue" $ nf queueViaQueue q
    ]
