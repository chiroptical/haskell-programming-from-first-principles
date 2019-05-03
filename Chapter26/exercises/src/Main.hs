module Main where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Functor.Identity
import System.IO.Unsafe

rDec :: Num a => Reader a a
-- rDec = reader $ \r -> r - 1
rDec = reader $ flip (-) 1

rShow :: Show a => ReaderT a Identity String
-- rShow = ReaderT $ \a -> Identity $ show a
rShow = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
  putStrLn $ "Hi: " ++ show a
  return $ a + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  let toString = show s
  putStrLn $ "Hi: " ++ toString
  return (toString, s + 1)

isValid :: String -> Bool
isValid = elem '!'

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "get more pumped!"
    Just e -> putStrLn $ e ++ " was very excite"

main :: IO ()
main = do
  -- print $ runReader rDec 1
  -- print $ fmap (runReader rDec) [1..10]
  -- print $ runReaderT rShow 1
  -- print $ fmap (runReaderT rShow) [1..10]
  -- a <- runReaderT rPrintAndInc 1
  -- print a
  -- as <- traverse (runReaderT rPrintAndInc) [1..10]
  -- print as
  b <- runStateT sPrintIncAccum 10
  print b
  bs <- mapM (runStateT sPrintIncAccum) [1..5]
  print bs
