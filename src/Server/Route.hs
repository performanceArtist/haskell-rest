module Server.Route (Scheme(..)) where

import Network.HTTP.Types (Method)

import Server.Url (Path)

data Scheme = Scheme {
  method :: Method,
  path :: Path
} deriving (Show)
