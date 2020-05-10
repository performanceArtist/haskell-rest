module Controller.Routes.App (routes) where

import Server.Handler (RootHandler)
import qualified Server.Route as Route
import qualified Controller.Routes.Test as Test
import qualified Controller.Routes.User as User

routes :: [(Route.Scheme, RootHandler)]
routes = Test.routes ++ User.routes
