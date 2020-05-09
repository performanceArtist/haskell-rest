module Model.User.Query (Create(..)) where

import GHC.Generics (Generic(..))
import Data.Aeson (FromJSON)

data Create = Create {
  username :: String,
  password :: String
} deriving (Show, Generic, FromJSON)
