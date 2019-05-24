{-# LANGUAGE BangPatterns #-}

module ManualBang where

doesntEval :: Bool -> Int
doesntEval b = 1

manualSeq :: Bool -> Int
manualSeq b = b `seq` 1

manualBang :: Bool -> Int
manualBang !b = 1

data Foo = Foo !Int !Int

first (Foo x _) = x
second (Foo _ y) = y