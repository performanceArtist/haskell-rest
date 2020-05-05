module Controller.Home (routes) where

import Network.HTTP.Types (status200)
import Control.Monad.Reader (asks, ask)
import Control.Monad.IO.Class (liftIO)

import Server.Handler (RouteHandler, Env(..))
import Server.Route (Route(..))
import Model.User (getByUsername)

home :: RouteHandler
home _ = return (status200, [("Big", "Chungus")], "Home route")

test :: RouteHandler
test _ = return (status200, [], "Test")

testID :: RouteHandler
testID params = do
  e <- ask
  let response = unlines [show params, show e]
  return (status200, [], response)

dbTest :: RouteHandler
dbTest _ = do
  conn' <- asks conn
  user <- liftIO $ getByUsername conn' "admin"
  return (status200, [], show user)

routes :: [(Route, RouteHandler)]
routes =
  [
    (Route "GET" "/", home),
    (Route "GET" "/test", test),
    (Route "GET" "/test/:id", testID),
    (Route "GET" "/db", dbTest)
  ]
