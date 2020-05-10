module Controller.Routes.User (routes) where

import Network.HTTP.Types (status200, status404, status400)
import Control.Monad.Reader (asks)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)

import Server.Handler (Handler, RootHandler, Response)
import qualified Server.Env as Env
import qualified Server.Route as Route
import Controller.Utils (prefix)
import Controller.Validation (withInt, extractBody)

import qualified Model.User.Field as Field
import qualified Model.User.Access as Access
import qualified Model.User.Update as Update
import qualified Model.User.Query.Create as Query.Create

getUser' :: Handler (Maybe Int) Response
getUser' (Just userID) = do
  conn <- asks Env.conn
  user <- liftIO $ Access.getByID conn userID
  return $ userResponse user
getUser' Nothing = return (status400, [], "Invalid id")

userResponse :: Maybe Field.User -> Response
userResponse (Just user) = (status200, [("Content-type", "application/json")], Field.toString user)
userResponse Nothing = (status404, [], "User not found")

getUser :: RootHandler
getUser = (withInt "user_id") >=> getUser'

postUser' :: Handler (Maybe Query.Create.Scheme) Response
postUser' (Just query') = do
  conn <- asks Env.conn
  liftIO $ Update.createUser conn query'
  return (status200, [], "Ok")
postUser' Nothing = return (status400, [], "Invalid query")

postUser :: RootHandler
postUser = (extractBody Query.Create.parse) >=> postUser'

routes :: [(Route.Scheme, RootHandler)]
routes = prefix "/user" $
  [
    (Route.Scheme "GET" "/:user_id", getUser),
    (Route.Scheme "POST" "/", postUser)
  ]
