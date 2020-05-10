module Model.User.Query.Create (Scheme(..), parse) where

import GHC.Generics (Generic(..))
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BSL

data Scheme = Scheme {
  username :: String,
  password :: String
} deriving (Show, Generic, FromJSON)

parse :: String -> Maybe Scheme
parse = decode . BSL.pack
