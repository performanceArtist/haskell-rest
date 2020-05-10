module Server.Handler (
  dispatcher,
  RootHandler,
  Response,
  Handler
) where

import Control.Monad.Reader (ReaderT, asks)
import Network.HTTP.Types
  (
    Status,
    Method,
    status404,
    ResponseHeaders
  )

import Server.Url (Path, UrlParams, matchStrict)
import qualified Server.Env as Env
import qualified Server.Route as Route

type ResponseBody = String
type Response = (Status, ResponseHeaders, ResponseBody)
type Handler a b = a -> ReaderT Env.Scheme IO b
type RootHandler = Handler UrlParams Response

matchRoute :: Route.Scheme -> Path -> Method -> Maybe UrlParams
matchRoute route path method =
  if Route.method route /= method
    then Nothing
    else matchStrict (Route.path route) path

dispatcher :: Handler ([(Route.Scheme, RootHandler)]) Response
dispatcher [] = return notFound
dispatcher ((route, handler):rest) = do
  path <- asks Env.path
  method <- asks Env.method
  case matchRoute route path method of
    Just params -> handler params
    _ -> dispatcher rest

notFound :: Response
notFound = (status404, [], "<h2>404</h2>")
