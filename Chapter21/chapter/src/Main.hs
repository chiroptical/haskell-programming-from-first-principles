module Main where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import HttpRequest
import RedGreen
import Two
import Identity
import Constant
import List
import Data.Monoid

-- [IO a] -> IO [a]

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query -- a :: [String]
  case sequence (map decodeFn a) of -- map decodeFn a :: [Either Err SomeObj], sequence ... :: Either Err [SomeObj]
    Left err -> return $ Left err 
    Right res -> do
      a <- makeIoOnlyObj res -- a :: [(SomeObj, IoOnlyObj)]
      return $ Right a

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)

pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = ((traverse makeIoOnlyObj . traverse decodeFn) =<<) . fetchFn
-- fetchFn _ :: IO [String]
-- (=<<) :: Monad m => (a -> m b) -> m a -> m b
-- m = IO
-- a = [String]
-- traverse makeIoOnlyObj . traverse decodeFn :: a -> m b
-- b = (Either Err [(SomeObj, IoOnlyObj)])
-- traverse makeIoOnlyObj :: Either Err [SomeObj] -> b
-- traverse decodeFn :: [String] -> Either Err [SomeObj]

t :: RedGreen Int (Int, String, Sum Int)
t = undefined

t' :: Two Int (Int, String, Sum Int)
t' = undefined

t'' :: Identity (Int, String, Sum Int)
t'' = undefined

t''' :: Constant (Sum Int) (Int, String, Sum Int)
t''' = undefined

t'''' :: List (Int, String, Sum Int)
t'''' = undefined

instance Eq a => EqProp (Sum a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ traversable t
  quickBatch $ traversable t'
  quickBatch $ traversable t''
  quickBatch $ traversable t'''
  quickBatch $ traversable t''''