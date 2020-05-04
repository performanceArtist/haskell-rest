module Server.Handler (dispatcher, Env(..), RouteHandler, Route(..)) where

import Database.SQLite.Simple (Connection)
import Control.Monad.Reader (ReaderT, asks)
import Network.HTTP.Types
  (
    Status,
    Method,
    status200,
    status404,
    RequestHeaders,
    ResponseHeaders
  )
import Data.List (find)
import Data.Maybe (isJust, fromJust)

import Server.Url (Path, UrlParams, matchStrict)

instance Show Connection where
  show _ =  "DB connection"

data Env = Env {
  path :: Path,
  query :: [(String, String)],
  body :: String,
  conn :: Connection,
  headers :: RequestHeaders,
  method :: Method
} deriving (Show)

type ResponseBody = String
type Response = (Status, ResponseHeaders, ResponseBody)
type Handler a = ReaderT Env IO a
type RouteHandler = UrlParams -> Handler Response

data Route = Route {
  routeMethod :: Method,
  routePath :: Path
} deriving (Show)

matchRoute :: Route -> Path -> Method -> Maybe UrlParams
matchRoute route path method =
  if routeMethod route /= method
    then Nothing
    else matchStrict (routePath route) path

dispatcher :: [(Route, RouteHandler)] -> Handler Response
dispatcher [] = return notFound
dispatcher ((route, handle):rest) = do
  path <- asks path
  method <- asks method
  let urlData = matchRoute route path method
  if isJust urlData
    then handle (fromJust urlData)
    else dispatcher rest

notFound = (status404, [], "<h2>404</h2>")
