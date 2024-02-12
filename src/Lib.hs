{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Context as C
import qualified Product.Controller as ProductController
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Logger
import Control.Monad.IO.Class

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]
  :<|> ProductController.API

startApp :: IO ()
startApp = runStderrLoggingT $ do
  context <- getContext
  liftIO $ run 8080 (app context)

app :: C.Context -> Application
app context = serve api (server context)

api :: Proxy API
api = Proxy

server :: C.Context -> Server API
server context = return users
  :<|> ProductController.server context

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
