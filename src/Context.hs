{-# LANGUAGE TemplateHaskell #-}

module Context(
    Context(Context, getPfdyDbPool, getAppBridgeDbPool),
    getContext
) where

import DB
import Database.Persist.Postgresql
import Data.Pool
import Data.Configurator
import Data.Configurator.Types
import Control.Concurrent
import Paths_app_bridge_back
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource

data Context = Context {
    getConfig :: Config
  , getPfdyDbPool :: Pool SqlBackend
  , getAppBridgeDbPool :: Pool SqlBackend
  }

getContext :: (MonadUnliftIO m, MonadLoggerIO m) => m Context
getContext = do
    (config, _) <- liftIO $ getConfig'
    appBridgeDbPool <- getAppBridgeDbPool' config
    pfdyDbPool <- getPfdyDbPool' config
    return $ Context {
        getConfig = config
      , getPfdyDbPool = appBridgeDbPool
      , getAppBridgeDbPool = pfdyDbPool
    }


getConfig' :: IO (Config, ThreadId)
getConfig' = liftIO $ do
    filePath <- getDataFileName "src/resources/application.cfg"
    autoReload autoConfig [Required filePath]