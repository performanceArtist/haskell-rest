module Controller.Validation (withInt) where

import Control.Monad ((>=>))
import Text.Read (readMaybe)
import Server.Handler (Handler)
import Server.Url (UrlParams)

withInt :: String -> UrlParams -> Handler (Maybe Int)
withInt name = return . (lookup name >=> readMaybe)
