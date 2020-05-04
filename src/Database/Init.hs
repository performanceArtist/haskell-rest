module Database.Init (withConn, testQuery) where

import Database.SQLite.Simple (open, close, Connection)
import Control.Exception (bracket)

import Database.User (UserField(..), getByUsername)

withConn = bracket (open "src/Database/migrations/test.db") close

testQuery :: Connection -> IO String
testQuery conn = do
  user <- getByUsername conn "admin"
  return $ show user
