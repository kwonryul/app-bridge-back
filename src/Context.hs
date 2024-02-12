{-# LANGUAGE TemplateHaskell #-}

module Context(
    Context(Context, getPfdyDbPool, getAppBridgeDbPool),
    getContext
) where

import DB
import Data.Configurator
import Data.Configurator.Types
import Control.Concurrent
import Paths_app_bridge_back
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource

data Context = Context {
    getConfig :: Config
  , getPfdyDbPool :: PfdyPool
  , getAppBridgeDbPool :: AppBridgePool
  }

getContext :: (MonadUnliftIO m, MonadLoggerIO m) => m Context
getContext = do
    (config, _) <- liftIO $ getConfig'
    appBridgeDbPool <- getAppBridgeDbPool' config
    pfdyDbPool <- getPfdyDbPool' config
    return $ Context {
        getConfig = config
      , getPfdyDbPool = pfdyDbPool
      , getAppBridgeDbPool = appBridgeDbPool
    }


getConfig' :: IO (Config, ThreadId)
getConfig' = liftIO $ do
    filePath <- getDataFileName "resources/application.cfg"
    autoReload autoConfig [Required filePath]