module Controller.Routes.User (routes) where

import Network.HTTP.Types (status404, status400)
import Control.Monad.Reader (asks)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)

import Server.Handler (Handler, RootHandler, Response)
import qualified Server.Env as Env
import qualified Server.Route as Route
import Controller.Utils (prefix, json, fromEither)
import Controller.Validation (withInt, extractBody)

import qualified Model.User.Field as Field
import qualified Model.User.Access as Access
import qualified Model.User.Update as Update
import qualified Model.User.Query.Create as Query.Create

getUser' :: Handler (Maybe Int) Response
getUser' (Just userID) = do
  conn <- asks Env.conn
  user <- liftIO $ Access.getByID conn userID
  return $ maybe (status404, [], "User not found") (json . Field.toString) user
getUser' Nothing = return (status400, [], "Invalid id")

getUser :: RootHandler
getUser = (withInt "user_id") >=> getUser'

postUser' :: Handler (Maybe Query.Create.Scheme) Response
postUser' (Just query) = do
  conn <- asks Env.conn
  result <- liftIO $ Update.createUser conn query
  return $ fromEither result
postUser' Nothing = return (status400, [], "Invalid query")

postUser :: RootHandler
postUser = (extractBody Query.Create.parse) >=> postUser'

routes :: [(Route.Scheme, RootHandler)]
routes = prefix "/user" $
  [
    (Route.Scheme "GET" "/:user_id", getUser),
    (Route.Scheme "POST" "/", postUser)
  ]
