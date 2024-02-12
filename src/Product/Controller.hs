{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Product.Controller(
    API,
    server
) where

import Context as C
import Product.Dto
import qualified Product.Service as ProductService

import Servant
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

type API = GetProduct

server :: C.Context -> Server API
server context = getProduct context

type GetProduct = Capture "id1" Int
    :> Capture "id2" Int
    :> Get '[JSON] (Maybe ResGetProduct)

getProduct' :: (MonadUnliftIO m) => C.Context -> Int -> Int -> m (Maybe ResGetProduct)
getProduct' context x y = ProductService.getProduct (getPfdyDbPool context) (getAppBridgeDbPool context) x y

getProduct :: C.Context -> Int -> Int -> Handler (Maybe ResGetProduct)
getProduct context x y = liftIO $ getProduct' context x y