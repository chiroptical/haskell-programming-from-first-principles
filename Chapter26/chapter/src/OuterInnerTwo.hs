module OuterInnerTwo where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Integer
embedded = return 1

start :: Either String (Maybe Integer)
start = Right (Just 1)

stepOne :: IO (Either String (Maybe Integer))
stepOne = pure start

stepTwo :: () -> IO (Either String (Maybe Integer))
stepTwo = const stepOne

stepThree :: ReaderT () IO (Either String (Maybe Integer))
stepThree = ReaderT stepTwo

stepFour :: ExceptT String (ReaderT () IO) (Maybe Integer)
stepFour = ExceptT stepThree

stepFive :: MaybeT (ExceptT String (ReaderT () IO)) Integer
stepFive = MaybeT stepFour

start' :: () -> Either String (Maybe Integer)
start' = const (Right (Just 1))

-- stepOne' :: MaybeT (ExceptT String (ReaderT () IO)) Integer
-- stepOne' = MaybeT $ ExceptT $ ReaderT $ const $ pure $ ($ ()) $ start'

stepOne' :: IO (Either String (Maybe Integer))
stepOne' = pure $ ($ ()) $ start'

stepTwo' :: () -> IO (Either String (Maybe Integer))
stepTwo' = const $ stepOne'

stepThree' :: ReaderT () IO (Either String (Maybe Integer))
stepThree' = ReaderT stepTwo'

stepFour' :: ExceptT String (ReaderT () IO) (Maybe Integer)
stepFour' = ExceptT stepThree'

stepFive' :: MaybeT (ExceptT String (ReaderT () IO)) Integer 
stepFive' = MaybeT stepFour'

embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Integer
embedded' = MaybeT . ExceptT . ReaderT . const . pure . ($ ()) . const . Right $ Just 1