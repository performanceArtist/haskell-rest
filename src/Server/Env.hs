module Server.Env (Env(..)) where

import Network.HTTP.Types (Method, RequestHeaders)
import Database.SQLite.Simple (Connection)
import Server.Url (Path)

data Env = Env {
  path :: Path,
  query :: [(String, String)],
  body :: String,
  conn :: Connection,
  headers :: RequestHeaders,
  method :: Method
}

instance Show Env where
  show e = unlines $ [(show . path) e, (show . query) e, (show . headers) e]
