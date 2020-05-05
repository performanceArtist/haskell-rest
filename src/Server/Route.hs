module Server.Route (Route(..)) where

import Network.HTTP.Types (Method)

import Server.Url (Path)

data Route = Route {
  method :: Method,
  path :: Path
} deriving (Show)
