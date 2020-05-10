module Controller.Utils (prefix, json, fromEither) where

import Network.HTTP.Types (status200, status500)

import Server.Handler (RootHandler, Response)
import qualified Server.Route as Route

prefix :: String -> [(Route.Scheme, RootHandler)] -> [(Route.Scheme, RootHandler)]
prefix prefix' routes = fmap (addPrefix prefix') routes

addPrefix :: String -> (Route.Scheme, RootHandler) -> (Route.Scheme, RootHandler)
addPrefix prefix' (route, handler) = (prefixed, handler)
  where
    oldPath = Route.path route
    newPath = if oldPath == "/" then prefix' else prefix' ++ oldPath
    prefixed = Route.Scheme { method = Route.method route, path = newPath }

json :: String -> Response
json response = (status200, [("Content-type", "application/json")], response)

fromEither :: Either String () -> Response
fromEither (Left e) = (status500, [], "Error: " ++ e)
fromEither _ = (status200, [], "Ok")
