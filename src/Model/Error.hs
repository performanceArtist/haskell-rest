module Model.Error (tryExecute) where

import Database.SQLite.Simple (SQLError)
import Control.Exception (try)
import Data.Bifunctor (first)

tryExecute :: IO () -> IO (Either String ())
tryExecute action = do
  result <- try action :: IO (Either SQLError ())
  return $ first (const "Something went wrong") result
