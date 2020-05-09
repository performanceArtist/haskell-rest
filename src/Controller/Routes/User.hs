module Controller.Routes.User (routes) where

import Network.HTTP.Types (status200, status404, status400)
import Control.Monad.Reader (asks)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)

import Server.Handler (RouteHandler, Handler, Response, Env(..))
import Server.Route (Route(..))
import Controller.Utils (prefix)
import Controller.Validation (withInt, extractBody)
import qualified Model.User.Field as Field
import qualified Model.User.Access as Access
import qualified Model.User.Update as Update
import qualified Model.User.Query as Query

getUser' :: Maybe Int -> Handler Response
getUser' (Just userID) = do
  conn' <- asks conn
  user <- liftIO $ Access.getByID conn' userID
  return $ userResponse user
getUser' Nothing = return (status400, [], "Invalid id")

userResponse :: Maybe Field.User -> Response
userResponse (Just user) = (status200, [("Content-type", "application/json")], Field.toString user)
userResponse Nothing = (status404, [], "User not found")

getUser :: RouteHandler
getUser = (withInt "user_id") >=> getUser'

postUser' :: Maybe Query.Create -> Handler Response
postUser' (Just query') = do
  conn' <- asks conn
  liftIO $ Update.createUser conn' query'
  return (status200, [], "Ok")
postUser' Nothing = return (status400, [], "Invalid query")

postUser :: RouteHandler
postUser = (extractBody Update.parseQuery) >=> postUser'

routes :: [(Route, RouteHandler)]
routes = prefix "/user" $
  [
    (Route "GET" "/:user_id", getUser),
    (Route "POST" "/", postUser)
  ]
