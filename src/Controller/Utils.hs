module Controller.Utils (prefix) where

import Server.Handler (RootHandler)
import Server.Route (Route(..))

prefix :: String -> [(Route, RootHandler)] -> [(Route, RootHandler)]
prefix prefix' routes = fmap (addPrefix prefix') routes

addPrefix :: String -> (Route, RootHandler) -> (Route, RootHandler)
addPrefix prefix' (route, handler) = (prefixed, handler)
  where
    oldPath = path route
    newPath = if oldPath == "/" then prefix' else prefix' ++ oldPath
    prefixed = Route { method = method route, path = newPath }
