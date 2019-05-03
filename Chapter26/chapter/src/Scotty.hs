{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty

import Data.Monoid (mconcat)
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad (liftM)
import Control.Monad.Trans.State.Lazy hiding (get)

main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        -- lift $ print beam
        -- (ActionT 
        --     . (ExceptT . liftM Right)
        --     . ReaderT . const
        --     . \m -> StateT (\s -> do
        --                        a <- m
        --                        return (a, s))) $ putStrLn "hello"
        liftIO $ print beam
        html $
            mconcat [ "<h1>Scotty, "
                    , beam
                    , " me up!</h1>"]