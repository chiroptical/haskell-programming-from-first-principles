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

-- embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Integer
-- embedded' = MaybeT $ ExceptT $ ReaderT (const (Right (Just 1)))