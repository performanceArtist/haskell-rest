module Main where

import Database.SQLite.Simple (Connection)
import Network.Wai
  (
    Application,
    Request,
    responseLBS,
    requestMethod,
    requestHeaders,
    rawPathInfo,
    rawQueryString,
    strictRequestBody
  )
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Handler.Warp (run)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Database.Init (withConn)
import Server.Handler (dispatcher, Env(..))
import Server.Route (routes)
import Server.Url (safeParseQuery)

main :: IO ()
main = run 5000 (logStdout app)

app :: Application
app req respond = withConn $ \connection -> do
  stringBody <- fmap BSL.unpack (strictRequestBody req)
  let env = Env {
    path = (BS.unpack . rawPathInfo) req,
    query = (safeParseQuery . BS.unpack . rawQueryString) req,
    body = stringBody,
    method = requestMethod req,
    headers = requestHeaders req,
    conn = connection
  }
  (status, headers, response) <- runReaderT (dispatcher routes) env
  respond $ responseLBS status headers (BSL.pack response)
