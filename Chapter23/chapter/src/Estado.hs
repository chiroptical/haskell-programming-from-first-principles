{-# LANGUAGE InstanceSigs #-}

module Estado where

newtype Estado s a = Estado { runEstado :: s -> (a, s) }

evalEstado :: Estado s a -> s -> a
evalEstado = (fst .) . runEstado

instance Functor (Estado s) where
    -- fmap :: (a -> b) -> Estado s a -> Estado s b
    fmap f (Estado s) =
        Estado $ \r ->
            let (a, s') = s r
            in (f a, s')