module Controller.Validation (withInt, extractBody) where

import Control.Monad ((>=>))
import Text.Read (readMaybe)
import Control.Monad.Reader (asks)

import Server.Handler (Handler)
import Server.Url (UrlParams)
import qualified Server.Env as Env

withInt :: String -> Handler UrlParams (Maybe Int)
withInt name = return . (lookup name >=> readMaybe)

extractBody :: (String -> Maybe a) -> Handler b (Maybe a)
extractBody parser _ = do
  result <- asks (parser . Env.body)
  return result
