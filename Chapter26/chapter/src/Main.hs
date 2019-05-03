{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor
import Data.Functor.Identity (Identity)
import OuterInner
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  MaybeT fma <*> MaybeT ma = MaybeT $ (<*>) <$> fma <*> ma

-- Initial instance working from ma and f
-- instance Monad m => Monad (MaybeT m) where
--   return = pure
--   MaybeT ma >>= f = MaybeT $
--     (fmap . fmap) f ma >>=
--       \mma -> case mma of
--         Just mb -> runMaybeT mb
--         Nothing -> return Nothing

instance Monad m => Monad (MaybeT m) where
  return = pure
  MaybeT mma >>= f = MaybeT $
    mma >>= \ma -> case ma of
      Nothing -> pure Nothing
      Just x -> runMaybeT (f x)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  EitherT fmea <*> EitherT mea = EitherT $ (<*>) <$> fmea <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT mea >>= f = EitherT $ do
    ea <- mea
    case ea of
      Left e -> return $ Left e
      Right a -> runEitherT (f a)

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ f <$> ema
  where
    f (Right x) = Left x
    f (Left y) = Right y

eitherT :: Monad m =>
              (a -> m c)
           -> (b -> m c)
           -> EitherT a m b
           -> m c
eitherT f g (EitherT amb) =
  amb >>= \e ->
    case e of
      Left x -> f x
      Right y -> g y

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  -- fmap f (ReaderT rma) = ReaderT $ \r -> f <$> rma r
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  ReaderT frma <*> ReaderT rma = ReaderT $ (<*>) <$> frma <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  ReaderT rma >>= f =
    ReaderT $ \r -> rma r >>=
      \a -> runReaderT (f a) r
  -- ReaderT rma >>= f = ReaderT $ \r -> do
  --   a <- rma r
  --   runReaderT (f a) r

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  -- fmap f (StateT sma) = StateT $ \s -> (,s) <$> ((fmap . fmap) (f . fst) sma) s
  fmap f sma = StateT $ \s -> fmap (\(x, y) -> (f x, y)) (runStateT sma s)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  fsma <*> sma = StateT $ \s -> do
    (f, s') <- runStateT fsma s 
    (a, s'') <- runStateT sma s'
    return (f a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  sma >>= f = StateT $ \s -> do
    (a, s') <- runStateT sma s
    runStateT (f a) s'

-- type Maybe a = MaybeT Identity a
-- type Either e a = EitherT e Identity a
-- type Reader r a = ReaderT r Identity a
type State s a = StateT s Identity a

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . liftM Right

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  -- lift ma = StateT $ (\s -> ma >>= \a -> return (a, s))
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

main :: IO ()
main = do
  putStrLn "hello world"
