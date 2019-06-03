{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Queue where

import           Control.DeepSeq
import           GHC.Generics    (Generic)

data Queue a =
  Queue
    { front :: [a]
    , back  :: [a]
    }
  deriving (Eq, Show, Generic, NFData)

push :: a -> Queue a -> Queue a
push x (Queue f b) = Queue (x : f) b

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue xs []) =
  let q' = Queue [] (reverse xs)
   in pop q'
pop (Queue f (b:bs)) = Just (b, Queue f bs)
