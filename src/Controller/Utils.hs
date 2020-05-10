module Controller.Utils (prefix) where

import Server.Handler (RootHandler)
import qualified Server.Route as Route

prefix :: String -> [(Route.Scheme, RootHandler)] -> [(Route.Scheme, RootHandler)]
prefix prefix' routes = fmap (addPrefix prefix') routes

addPrefix :: String -> (Route.Scheme, RootHandler) -> (Route.Scheme, RootHandler)
addPrefix prefix' (route, handler) = (prefixed, handler)
  where
    oldPath = Route.path route
    newPath = if oldPath == "/" then prefix' else prefix' ++ oldPath
    prefixed = Route.Scheme { method = Route.method route, path = newPath }
