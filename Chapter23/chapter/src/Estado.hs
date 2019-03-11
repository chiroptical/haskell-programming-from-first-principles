{-# LANGUAGE InstanceSigs #-}

module Estado where

newtype Estado s a = Estado { runEstado :: s -> (a, s) }

(...) = (.) . (.)

evalEstado :: Estado s a -> s -> a
-- evalEstado = (fst .) . runEstado
evalEstado = fst ... runEstado

instance Functor (Estado s) where
    fmap f (Estado s) =
        Estado $ \r ->
            let (a, s') = s r
            in (f a, s')

instance Applicative (Estado s) where
    pure x = Estado $ \r -> (x, r)
    Estado fsa <*> Estado sa = Estado $ \r ->
        let (f, s) = fsa r
            (a, s') = sa s
        in (f a, s')

instance Monad (Estado s) where
    return = pure
    Estado f >>= g = Estado $ \r ->
        let (a, s) = f r
        in (runEstado $ g a) s