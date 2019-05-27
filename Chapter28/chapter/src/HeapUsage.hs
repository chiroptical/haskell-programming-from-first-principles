module HeapUsage where

import Control.Monad

blah :: [Integer]
blah = [1..1000]

runHeapUsage :: IO ()
runHeapUsage = replicateM_ 10000 (print blah)