module Controller.Routes.App (routes) where

import Server.Handler (RootHandler)
import Server.Route (Route(..))
import qualified Controller.Routes.Test as Test
import qualified Controller.Routes.User as User

routes :: [(Route, RootHandler)]
routes = Test.routes ++ User.routes
