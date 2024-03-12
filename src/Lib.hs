{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Context as C
import qualified Product.Controller as ProductController
import Middleware
import Paths_app_bridge_back
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Static.TH.Internal.Mime
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.ByteString as BS
import System.IO

type API = "favicon.ico" :> Get '[ICO] ByteString
  :<|> "product" :> ProductController.API
  :<|> "static" :> Raw

startApp :: IO ()
startApp = runStderrLoggingT $ do
  context <- getContext
  filePath <- liftIO $ getDataFileName "resources/static"
  liftIO $ run 8083 (app context filePath)

app :: C.Context -> FilePath -> Application
app context filePath = corsMiddleware $ serve api (server context filePath)

api :: Proxy API
api = Proxy

server :: C.Context -> FilePath -> Server API
server context filePath = faviconServer
  :<|> ProductController.server context
  :<|> serveDirectoryWebApp filePath

faviconServer :: Handler ByteString
faviconServer = liftIO $ do
    filePath <- getDataFileName "resources/images/favicon.ico"
    handle <- openFile filePath ReadMode
    contents <- BS.hGetContents handle
    hClose handle
    return contents