module Product.Service(
    getProduct
) where

import Product.Model
import Product.Dto
import qualified Product.Repository as ProductRepository

import DB
import Database.Persist
import Database.Persist.Typed
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

getProduct' :: (MonadUnliftIO m) => PfdyConn -> AppBridgeConn -> Int ->  m (Maybe ResGetProduct)
getProduct' conn conn' x = do
    liftIO $ print "hi"
    productEntityWithUser' <- ProductRepository.getProduct conn conn' x
    case productEntityWithUser' of
        Just (Entity productId produkt, _) -> do
            case toPersistValue productId of
                PersistInt64 productId' -> 
                    return $ Just $ ResGetProduct (fromIntegral productId') (productName produkt) (productPrice produkt)
                _ -> return Nothing
        Nothing -> return Nothing

getProduct :: (MonadUnliftIO m) => PfdyPool -> AppBridgePool -> Int -> Int -> m (Maybe ResGetProduct)
getProduct pfdyDbPool appBridgeDbPool x y = do
    runSqlPoolFor (ReaderT (\conn ->
        runSqlPoolFor (ReaderT (\conn' -> do
            getProduct' conn conn' (x + y)
        )) appBridgeDbPool
     )) pfdyDbPool