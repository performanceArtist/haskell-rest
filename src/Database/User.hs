module Database.User (UserField(..), getByUsername) where

import Control.Applicative ((<*>))
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import Database.SQLite.Simple (Connection, query, Only(..))

data UserField = UserField {
  id :: Int,
  username :: String,
  password :: String,
  session_id :: Maybe String
} deriving (Show)

instance FromRow UserField where
  fromRow = UserField <$> field <*> field <*> field <*> field

getByUsername :: Connection -> String -> IO (Maybe UserField)
getByUsername conn username = do
  rows <- query conn "SELECT * FROM user WHERE username = ?" (Only username) :: IO [UserField]
  return $ getFirst rows

getFirst :: [a] -> Maybe a
getFirst [a] = Just a
getFirst _ = Nothing
