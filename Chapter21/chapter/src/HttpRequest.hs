module HttpRequest where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "https://twitch.tv/chiroptical"
       , "https://barry-moore-ii.com"
       , "http://haskellbook.com"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedGet :: IO [Response ByteString]
traversedGet = traverse get urls