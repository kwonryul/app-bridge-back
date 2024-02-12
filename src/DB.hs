{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB(
    getAppBridgeDbPool',
    getPfdyDbPool',
    setSchema
) where

import Database.Persist.Postgresql
import Data.Pool
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Text as Text
import Data.Configurator as Cfg
import Data.Configurator.Types
import Data.ByteString.Char8 as BS

appBridgeConnStr' :: Config -> IO ConnectionString
appBridgeConnStr' config = do
    host' <- Cfg.lookup config "db.appBridge.host"
    port' <- Cfg.lookup config "db.appBridge.port"
    dbname' <- Cfg.lookup config "db.appBridge.dbname"
    user' <- Cfg.lookup config "db.appBridge.user"
    password' <- Cfg.lookup config "db.appBridge.password"
    case genConnStr <$> host' <*> port' <*> dbname' <*> user' <*> password' of
        Just connStr -> return connStr
        Nothing -> error "DB config file invalid"

pfdyConnStr' :: Config -> IO ConnectionString
pfdyConnStr' config = do
    host' <- Cfg.lookup config "db.pfdy.host"
    port' <- Cfg.lookup config "db.pfdy.port"
    dbname' <- Cfg.lookup config "db.pfdy.dbname"
    user' <- Cfg.lookup config "db.pfdy.user"
    password' <- Cfg.lookup config "db.pfdy.password"
    case genConnStr <$> host' <*> port' <*> dbname' <*> user' <*> password' of
        Just connStr -> return connStr
        Nothing -> error "DB config file invalid"

genConnStr :: String -> Int -> String -> String -> String -> ConnectionString
genConnStr host port dbname user password = BS.pack ("host=" ++ host ++ " port=" ++ (show port) ++ " dbname=" ++ dbname ++ " user=" ++ user ++ " password=" ++ password)

getAppBridgeDbPool' :: (MonadUnliftIO m, MonadLoggerIO m) => Config -> m (Pool SqlBackend)
getAppBridgeDbPool' config = do
    appBridgeConnStr <- liftIO $ appBridgeConnStr' config
    createPostgresqlPool appBridgeConnStr 8

getPfdyDbPool' :: (MonadUnliftIO m, MonadLoggerIO m) => Config -> m (Pool SqlBackend) 
getPfdyDbPool' config = do
    pfdyConnStr <- liftIO $ pfdyConnStr' config
    createPostgresqlPool pfdyConnStr 8

setSchema :: MonadIO m => String -> SqlPersistT m ()
setSchema schema = rawExecute "SET search_path TO ?" [PersistText $ Text.pack schema] 