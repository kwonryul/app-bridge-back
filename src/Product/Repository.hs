module Product.Repository(
    getProduct
) where

import Product.Model
import User.Model

import DB
import Database.Persist.Typed
import Control.Monad.Trans.Resource
import Database.Persist
import Database.Persist.Sql
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

getProduct :: (MonadUnliftIO m) => PfdyConn -> AppBridgeConn -> Int -> m (Maybe (Entity Product, Maybe User))
getProduct conn conn' x = do
    runReaderT (runMigration migrateUser) (generalizeSqlBackend conn)
    runReaderT (runMigration migrateProduct) (generalizeSqlBackend conn')
    productEntityList <- runReaderT (selectList [ProductPrice ==. x] []) conn'
    let productEntity' = case productEntityList of
                        (e : []) -> 
                            Just e
                        _ -> Nothing
    user' <- case productEntity' of
        Just (Entity _ (Product _ _ (Just userId))) ->
            do
                user'' <- runReaderT (get userId) conn
                liftIO $ print userId
                case user'' of
                    Just user -> liftIO $ print (userAge user)
                    _ -> return ()
                return user''
        _ -> return Nothing
    case productEntity' of
        Just productEntity -> return $ Just (productEntity, user')
        Nothing -> return Nothing