module Profiling where

-- m a >>= a -> m b
-- m (m b)
-- m b

f :: IO ()
f = do
  print ([1..] !! 999999)
  putStrLn "f"

g :: IO ()
g = do
  print ([1..] !! 99999)
  putStrLn "g"

runProfiling :: IO ()
runProfiling = do
  f
  g
