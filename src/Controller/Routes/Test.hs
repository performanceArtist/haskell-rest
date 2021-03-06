module Controller.Routes.Test (routes, BigQuery(..)) where

import Network.HTTP.Types (status200, status400)
import Control.Monad.Reader (ask, asks)
import Control.Monad ((>=>))

import Server.Handler (RootHandler, Handler, Response)
import qualified Server.Env as Env
import qualified Server.Route as Route
import Controller.Validation (withInt)
import Server.Url (UrlParams)

test :: RootHandler
test _ = return (status200, [("Big", "Chungus")], "Test")

testID' :: Handler (Maybe Int) Response
testID' (Just validID) = do
  e <- ask
  let response = unlines ["ID:" ++ (show validID), show e]
  return (status200, [], response)
testID' Nothing = return (status400, [], "Invalid id")

testID :: RootHandler
testID = testID' . (withInt "id")

data BigQuery = BigQuery {
  base_id :: Int,
  item_id :: Int,
  one :: Int,
  two :: String
} deriving (Show)

parseBigQuery :: Handler UrlParams (Maybe BigQuery)
parseBigQuery params = do
  let baseID = withInt "id" params
  let itemID = withInt "item_id" params
  query <- asks Env.query
  let one' = withInt "one" query
  let two' = lookup "two" query
  return $ BigQuery <$> baseID <*> itemID <*> one' <*> two'

testQueryParam' :: Handler (Maybe BigQuery) Response
testQueryParam' (Just bigQuery) = return (status200, [], show bigQuery)
testQueryParam' Nothing = return (status400, [], "Invalid query")

testQueryParam :: RootHandler
testQueryParam = parseBigQuery >=> testQueryParam'

routes :: [(Route.Scheme, RootHandler)]
routes =
  [
    (Route.Scheme "GET" "/test", test),
    (Route.Scheme "GET" "/test/:id", testID),
    (Route.Scheme "GET" "/test/:id/item/:item_id", testQueryParam)
  ]
