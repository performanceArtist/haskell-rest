module Model.User (UserField(..), getByUsername) where

import Database.SQLite.Simple.FromRow (FromRow(..), field)
import Database.SQLite.Simple (Connection, query, Only(..))
import Data.Maybe (listToMaybe)

data UserField = UserField {
  id :: Int,
  username :: String,
  password :: String,
  session_id :: Maybe String
} deriving (Show)

instance FromRow UserField where
  fromRow = UserField <$> field <*> field <*> field <*> field

getByUsername :: Connection -> String -> IO (Maybe UserField)
getByUsername conn name = do
  rows <- query conn "SELECT * FROM user WHERE username = ?" (Only name) :: IO [UserField]
  return $ listToMaybe rows
