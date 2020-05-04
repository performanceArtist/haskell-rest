module Server.Route (routes) where

import Network.HTTP.Types (status200)
import Control.Monad.Reader (asks, ask)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)

import Server.Handler (RouteHandler, Route(..), Env(..))
import Database.Init (testQuery)

homeRoute = Route { routeMethod = "GET", routePath = "/" }
home :: RouteHandler
home _ = return (status200, [("Big", "Chungus")], "Home route")

testRoute = Route { routeMethod = "GET", routePath = "/test" }
test :: RouteHandler
test _ = return (status200, [], "Test")

testIDRoute = Route { routeMethod = "GET", routePath = "/test/:id" }
testID :: RouteHandler
testID params = do
  e <- ask
  let response = intercalate "\n" [show params, show e]
  return (status200, [], response)

dbRoute = Route { routeMethod = "GET", routePath = "/db" }
dbTest :: RouteHandler
dbTest _ = do
  conn <- asks conn
  liftIO $ fmap (\rows -> (status200, [], rows)) (testQuery conn)

userGetRoute = Route { routeMethod = "GET", routePath = "/user" }
userGet :: RouteHandler
userGet _ = do
  return (status200, [("Set-Cookie", "big=chungus")], "Ok")

userPostRoute = Route { routeMethod = "POST", routePath = "/user" }
userPost :: RouteHandler
userPost _ = do
  return (status200, [], "Ok")

userLoginRoute = Route { routeMethod = "POST", routePath = "/user/login" }
userLogin :: RouteHandler
userLogin _ = do
  body <- asks body
  return (status200, [], "Ok" ++ body)

routes :: [(Route, RouteHandler)]
routes =
  [
    (homeRoute, home),
    (testRoute, test),
    (testIDRoute, testID),
    (dbRoute, dbTest),
    (userGetRoute, userGet),
    (userPostRoute, userPost)
  ]
