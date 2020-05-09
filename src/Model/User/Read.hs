module Model.User.Read (getByID) where

import Database.SQLite.Simple (Connection, query, Only(..))
import Data.Maybe (listToMaybe)

import qualified Model.User.Field as Field

getByID :: Connection -> Int -> IO (Maybe Field.User)
getByID conn userID = do
  rows <- query conn "SELECT * FROM user WHERE id = ?" (Only userID) :: IO [Field.User]
  return $ listToMaybe rows
