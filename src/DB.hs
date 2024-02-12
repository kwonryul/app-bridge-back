{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB(
    PfdyDb
  , AppBridgeDb
  , PfdyConn
  , AppBridgeConn
  , PfdyPool
  , AppBridgePool
  , getAppBridgeDbPool'
  , getPfdyDbPool'
  , setSchema
) where

import Database.Persist.Postgresql
import Database.Persist.MySQL as MySQL
import Database.Persist.Typed
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Configurator as Cfg
import Data.Configurator.Types
import Data.ByteString.Char8 as BS

data PfdyDb
data AppBridgeDb

type PfdyConn = SqlFor PfdyDb
type AppBridgeConn = SqlFor AppBridgeDb

type PfdyPool = ConnectionPoolFor PfdyDb
type AppBridgePool = ConnectionPoolFor AppBridgeDb

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

genConnStr :: String -> Int -> String -> String -> String -> ConnectionString
genConnStr host port dbname user password = BS.pack ("host=" ++ host ++ " port=" ++ (show port) ++ " dbname=" ++ dbname ++ " user=" ++ user ++ " password=" ++ password)

pfdyConnInfo' :: Config -> IO MySQL.ConnectInfo
pfdyConnInfo' config = do
    host' <- Cfg.lookup config "db.pfdy.host"
    port' <- Cfg.lookup config "db.pfdy.port"
    dbname' <- Cfg.lookup config "db.pfdy.dbname"
    user' <- Cfg.lookup config "db.pfdy.user"
    password' <- Cfg.lookup config "db.pfdy.password"
    case do
        host <- host'
        port <- port'
        dbname <- dbname'
        user <- user'
        password <- password'
        return MySQL.defaultConnectInfo { MySQL.connectHost = host, MySQL.connectPort = port, MySQL.connectUser = user, MySQL.connectPassword = password, MySQL.connectDatabase = dbname }
        of
        Just connInfo -> return connInfo
        Nothing -> error "DB config file invalid"

getAppBridgeDbPool' :: (MonadUnliftIO m, MonadLoggerIO m) => Config -> m AppBridgePool
getAppBridgeDbPool' config = do
    appBridgeConnStr <- liftIO $ appBridgeConnStr' config
    specializePool <$> createPostgresqlPoolModified (setSchema "kwonryul") appBridgeConnStr 8

getPfdyDbPool' :: (MonadUnliftIO m, MonadLoggerIO m) => Config -> m PfdyPool
getPfdyDbPool' config = do
    pfdyConnInfo <- liftIO $ pfdyConnInfo' config
    specializePool <$> createMySQLPool pfdyConnInfo 8

setSchema :: String -> Connection -> IO ()
setSchema schema conn = execute conn "SET search_path TO ?" [schema] >> return ()