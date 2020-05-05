module Controller.User (routes) where

import Network.HTTP.Types (status200)
import Control.Monad.Reader (asks)

import Server.Handler (RouteHandler, Env(..))
import Server.Route (Route(..))
import Controller.Utils (prefix)

userGet :: RouteHandler
userGet _ = do
  return (status200, [("Set-Cookie", "big=chungus")], "Ok")

userPost :: RouteHandler
userPost _ = do
  return (status200, [], "Ok")

userLogin :: RouteHandler
userLogin _ = do
  body' <- asks body
  return (status200, [], "Ok" ++ body')

routes :: [(Route, RouteHandler)]
routes = prefix "/user" $
  [
    (Route "GET" "/", userGet),
    (Route "POST" "/", userPost),
    (Route "POST" "/login", userLogin)
  ]
