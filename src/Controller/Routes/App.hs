module Controller.Routes.App (routes) where

import Server.Handler (RouteHandler)
import Server.Route (Route(..))
import qualified Controller.Routes.Home as Home
import qualified Controller.Routes.User as User

routes :: [(Route, RouteHandler)]
routes = Home.routes ++ User.routes
