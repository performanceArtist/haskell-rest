module Controller.Utils (prefix) where

import Server.Handler (RouteHandler)
import Server.Route (Route(..))

prefix :: String -> [(Route, RouteHandler)] -> [(Route, RouteHandler)]
prefix prefix' routes = fmap (addPrefix prefix') routes

addPrefix :: String -> (Route, RouteHandler) -> (Route, RouteHandler)
addPrefix prefix' (route, handler) = (prefixed, handler)
  where
    oldPath = path route
    newPath = if oldPath == "/" then prefix' else prefix' ++ oldPath
    prefixed = Route { method = method route, path = newPath }
