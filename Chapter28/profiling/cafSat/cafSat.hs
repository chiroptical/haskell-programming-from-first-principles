module Main where

--incdInts :: [Integer]
--incdInts = map (+1) [1..]

incdInts :: [Integer] -> [Integer]
incdInts xs = map (+1) xs

main :: IO ()
main = do
  print (incdInts [1..] !! 1000)
  --print (incdInts !! 10000)
  --print (incdInts !! 100000)
  --print (incdInts !! 1000000)
  --print (incdInts !! 1500000)
  --print (incdInts !! 2000000)
