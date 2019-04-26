module Main where

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

main :: IO ()
main = do
  putStrLn "hello world"
