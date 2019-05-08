module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Integer
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Integer)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Integer))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Integer))
readerUnwrap = runReaderT eitherUnwrap

embedded1 :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded1 = MaybeT m1

m1 :: ExceptT String (ReaderT () IO) (Maybe Int)
m1 = ExceptT m2

m2 :: ReaderT () IO (Either String (Maybe Int))
m2 = ReaderT m3

m3 :: () -> IO (Either String (Maybe Int))
m3 _ = m4

m4 :: IO (Either String (Maybe Int))
m4 = pure m5

m5 :: Either String (Maybe Int)
m5 = Right (Just 1)

embedded2 :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded2 = MaybeT $ ExceptT $ ReaderT $ const $ pure $ Right (Just 1)

embedded3 :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded3 = MaybeT $ ExceptT $ ReaderT $ const $ pure
            $ ($ ())
            $ const (Right (Just 1))

embedded4 :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded4 = (MaybeT . ExceptT . ReaderT . const . pure . ($ ()))
  (const (Right (Just 1)))