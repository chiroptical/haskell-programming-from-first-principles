module DList where

newtype DList a =
  DL
    { unDL :: [a] -> [a]
    }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL $ (x:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList dl = (unDL dl) []
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs) 
{-# INLINE cons #-}

infixr `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs xs' = DL (unDL xs . unDL xs')
{-# INLINE append #-}