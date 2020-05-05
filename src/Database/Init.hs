module Database.Init (withConn) where

import Database.SQLite.Simple (open, close, Connection)
import Control.Exception (bracket)

withConn :: (Connection -> IO a) -> IO a
withConn = bracket (open "src/Database/db.db") close
