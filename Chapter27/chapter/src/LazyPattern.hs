module LazyPattern where

strictPattern :: (a, b) -> String
strictPattern (a, _) = const "refuttable" a

lazyPattern :: (a, b) -> String
lazyPattern ~(a, _) = const "irrefutable" a
