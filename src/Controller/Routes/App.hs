module Controller.Routes.App (routes) where

import Server.Handler (RouteHandler)
import Server.Route (Route(..))
import qualified Controller.Routes.Test as Test
import qualified Controller.Routes.User as User

routes :: [(Route, RouteHandler)]
routes = Test.routes ++ User.routes
