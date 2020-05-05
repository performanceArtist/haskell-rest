module Controller.App (routes) where

import Server.Handler (RouteHandler)
import Server.Route (Route(..))
import qualified Controller.Home as Home
import qualified Controller.User as User

routes :: [(Route, RouteHandler)]
routes = Home.routes ++ User.routes
