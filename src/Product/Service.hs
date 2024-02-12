module Product.Service(
    getProduct
) where

import Product.Model
import Product.Dto
import qualified Product.Repository as ProductRepository

import DB

import Database.Persist

import Data.Pool
import Database.Persist.Postgresql
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

getProduct' :: (MonadUnliftIO m) => SqlBackend -> SqlBackend -> Int ->  m (Maybe ResGetProduct)
getProduct' conn conn' x = do
    productEntityWithUser' <- ProductRepository.getProduct conn conn' x
    case productEntityWithUser' of
        Just (Entity productId produkt, _) -> do
            case toPersistValue productId of
                PersistInt64 productId' -> 
                    return $ Just $ ResGetProduct (fromIntegral productId') (productName produkt) (productPrice produkt)
                _ -> return Nothing
        Nothing -> return Nothing

getProduct :: (MonadUnliftIO m) => Pool SqlBackend -> Pool SqlBackend -> Int -> Int -> m (Maybe ResGetProduct)
getProduct pfdyDbPool appBridgeDbPool x y = do
    runSqlPool (ReaderT (\conn ->
        runSqlPool (ReaderT (\conn' -> do
            runReaderT (setSchema "sh") conn
            runReaderT (setSchema "kwonryul") conn'
            getProduct' conn conn' (x + y)
        )) appBridgeDbPool
     )) pfdyDbPool