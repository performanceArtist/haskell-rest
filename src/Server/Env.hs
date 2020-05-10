module Server.Env (Scheme(..)) where

import Network.HTTP.Types (Method, RequestHeaders)
import Database.SQLite.Simple (Connection)
import Server.Url (Path)

data Scheme = Scheme {
  path :: Path,
  query :: [(String, String)],
  body :: String,
  conn :: Connection,
  headers :: RequestHeaders,
  method :: Method
}

instance Show Scheme where
  show e = unlines $ [(show . path) e, (show . query) e, (show . headers) e]
