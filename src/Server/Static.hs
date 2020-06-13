module Server.Static (staticMiddleware) where

import Network.Wai.Middleware.Static (Policy(..), staticPolicy, addBase, policy, (>->))
import Network.Wai (Middleware)

indexPolicy :: Policy
indexPolicy = policy $ \url -> if url == ""
  then Just "index.html"
  else Just url

type FolderPath = String

publicPolicy :: FolderPath -> Policy
publicPolicy path = addBase path

staticMiddleware :: FolderPath -> Middleware
staticMiddleware path = staticPolicy $ indexPolicy >-> (publicPolicy path)
