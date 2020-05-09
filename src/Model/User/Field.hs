module Model.User.Field (User(..), toString) where

import Prelude hiding (id)
import GHC.Generics (Generic(..))
import Data.Aeson (ToJSON, toJSON, object, (.=), encode)
import Database.SQLite.Simple.FromRow (FromRow(..), field)

data User = User {
  id :: Int,
  username :: String,
  password :: String,
  session_id :: Maybe String
} deriving (Show, Generic)

instance ToJSON User where
  toJSON user = object [ "id" .= id user, "username" .= username user ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

toString :: User -> String
toString user = show $ encode user
