module Server.Handler (dispatcher, Env(..), RouteHandler) where

import Database.SQLite.Simple (Connection)
import Control.Monad.Reader (ReaderT, asks)
import Network.HTTP.Types
  (
    Status,
    Method,
    status404,
    RequestHeaders,
    ResponseHeaders
  )
import Data.Maybe (isJust, fromJust)

import Server.Url (Path, UrlParams, matchStrict)
import qualified Server.Route as Route

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

type ResponseBody = String
type Response = (Status, ResponseHeaders, ResponseBody)
type Handler a = ReaderT Env IO a
type RouteHandler = UrlParams -> Handler Response

matchRoute :: Route.Route -> Path -> Method -> Maybe UrlParams
matchRoute route path' method' =
  if Route.method route /= method'
    then Nothing
    else matchStrict (Route.path route) path'

dispatcher :: [(Route.Route, RouteHandler)] -> Handler Response
dispatcher [] = return notFound
dispatcher ((route, handle):rest) = do
  path' <- asks path
  method' <- asks method
  let params = matchRoute route path' method'
  if isJust params
    then handle (fromJust params)
    else dispatcher rest

notFound :: Response
notFound = (status404, [], "<h2>404</h2>")
