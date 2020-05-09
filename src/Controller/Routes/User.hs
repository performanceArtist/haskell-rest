module Controller.Routes.User (routes) where

import Network.HTTP.Types (status200, status404, status400)
import Control.Monad.Reader (asks)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)

import Server.Handler (RouteHandler, Handler, Response, Env(..))
import Server.Route (Route(..))
import Controller.Utils (prefix)
import Controller.Validation (withInt)
import Model.User.Field (User(..), toString)
import Model.User.Read (getByID)

getUser' :: Maybe Int -> Handler Response
getUser' (Just userID) = do
  conn' <- asks conn
  user <- liftIO $ getByID conn' userID
  return $ userResponse user
getUser' Nothing = return (status400, [], "Invalid id")

userResponse :: Maybe User -> Response
userResponse (Just user) = (status200, [("Content-type", "application/json")], toString user)
userResponse Nothing = (status404, [], "User not found")

getUser :: RouteHandler
getUser = (withInt "user_id") >=> getUser'

postUser :: RouteHandler
postUser _ = do
  return (status200, [], "Ok")

routes :: [(Route, RouteHandler)]
routes = prefix "/user" $
  [
    (Route "GET" "/:user_id", getUser),
    (Route "POST" "/", postUser)
  ]
