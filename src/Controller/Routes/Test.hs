module Controller.Routes.Test (routes) where

import Network.HTTP.Types (status200, status400)
import Control.Monad.Reader (ask)
import Control.Monad ((>=>))

import Server.Handler (RouteHandler, Handler, Response)
import Server.Route (Route(..))
import Controller.Validation (withInt)

test :: RouteHandler
test _ = return (status200, [("Big", "Chungus")], "Test")

testID' :: Maybe Int -> Handler Response
testID' (Just validID) = do
  e <- ask
  let response = unlines ["ID:" ++ (show validID), show e]
  return (status200, [], response)
testID' Nothing = return (status400, [], "Invalid id")

testID :: RouteHandler
testID = (withInt "id") >=> testID'

routes :: [(Route, RouteHandler)]
routes =
  [
    (Route "GET" "/test", test),
    (Route "GET" "/test/:id", testID)
  ]
