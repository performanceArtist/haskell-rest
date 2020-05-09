module Controller.Validation (withInt, extractBody) where

import Control.Monad ((>=>))
import Text.Read (readMaybe)
import Control.Monad.Reader (asks)

import Server.Handler (Handler, Env(..))
import Server.Url (UrlParams)

withInt :: String -> Handler UrlParams (Maybe Int)
withInt name = return . (lookup name >=> readMaybe)

extractBody :: (String -> Maybe a) -> Handler b (Maybe a)
extractBody parser _ = do
  result <- asks (parser . body)
  return result
