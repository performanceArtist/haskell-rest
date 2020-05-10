module Server.Static (staticMiddleware) where

import Network.Wai.Middleware.Static (Policy(..), staticPolicy, addBase, policy, (>->))
import Network.Wai (Middleware)

indexPolicy :: Policy
indexPolicy = policy $ \url -> if url == ""
  then Just "index.html"
  else Just url

publicPolicy :: Policy
publicPolicy = addBase "public"

staticMiddleware :: Middleware
staticMiddleware = staticPolicy $ indexPolicy >-> publicPolicy
