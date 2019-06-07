module Main where

import           Data.List
import qualified Data.Map           as M
import           ParseIni
import           System.Directory
import           Text.Trifecta

import           System.Environment
import           System.IO

parse :: String -> Result Config
parse = parseString parseINI mempty

usage :: String
usage = "stack run -- <directory>"

parseIfIni :: String -> String -> IO (String, Config)
parseIfIni dirname filename = do
  contents <- readFile $ dirname ++ "/" ++ filename
  let config = parse contents
  case config of
    Success c -> return (filename, c)
    _         -> error "Failure"

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then print usage
    else pure ()
  let dir = head args
  dirContents <- listDirectory dir
  let iniFiles = filter (isInfixOf ".ini") dirContents
  map <- sequenceA $ parseIfIni dir <$> iniFiles
  print $ M.fromList map