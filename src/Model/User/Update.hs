module Model.User.Update (parseQuery, createUser) where

import Database.SQLite.Simple (Connection, execute)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Model.User.Query as Query

parseQuery :: String -> Maybe Query.Create
parseQuery body = decode (BSL.pack body)

createUser :: Connection -> Query.Create -> IO ()
createUser conn query = do
  execute conn "INSERT INTO user (username, password) VALUES (?, ?)" (Query.username query, Query.password query)
