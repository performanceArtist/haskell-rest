module Model.User.Update (createUser) where

import Database.SQLite.Simple (Connection, execute)

import Model.Error (tryExecute)
import qualified Model.User.Query.Create as Query.Create

createUser :: Connection -> Query.Create.Scheme -> IO (Either String ())
createUser conn query = do
  let values = (Query.Create.username query, Query.Create.password query)
  tryExecute $ execute conn "INSERT INTO user (username, password) VALUES (?, ?)" values
