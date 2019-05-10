{-# LANGUAGE OverloadedStrings #-}

module AppTwo where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config { counts :: IORef (M.Map Text Integer)
         , prefix :: Text
         }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoop :: Text -> M.Map Text Integer -> M.Map Text Integer
bumpBoop k m = M.insertWith (+) k 1 m

app :: Scotty ()
app = get "/:key" $ do
  unprefixed <- param "key"
  config <- lift ask
  let key' = prefix config <> unprefixed
      ref = counts config
  map <- liftIO $ readIORef $ ref
  updatedMap <- liftIO $ pure $ bumpBoop key' map
  liftIO $ writeIORef ref updatedMap
  html $ mconcat [ "<h1>Success! Count is: "
                 , TL.pack $ show $ updatedMap M.! key' 
                 , "</h1>"
                 ]