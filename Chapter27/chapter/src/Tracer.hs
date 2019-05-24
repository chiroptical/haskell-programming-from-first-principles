module Tracer where

import           Debug.Trace (trace)

inc = (+ 1)

twice = inc . inc

howManyTimes =
  let opo = trace "compute one + one" (1 + 1)
   in inc (trace "one + one" opo) + twice (trace "one + one" opo)

howManyTimes' =
  let onePlusOne = trace "one + one" (1 + 1)
   in inc onePlusOne + inc onePlusOne

f :: a -> Int
f = trace "f" const 1  