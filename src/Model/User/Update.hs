module Model.User.Update (createUser) where

import Database.SQLite.Simple (Connection, execute)

import qualified Model.User.Query.Create as Query.Create

createUser :: Connection -> Query.Create.Scheme -> IO ()
createUser conn query = do
  execute conn "INSERT INTO user (username, password) VALUES (?, ?)" (Query.Create.username query, Query.Create.password query)
