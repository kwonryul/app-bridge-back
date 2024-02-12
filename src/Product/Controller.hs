{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Product.Controller(
    API,
    server
) where

import Header
import Context as C
import Product.Dto
import qualified Product.Service as ProductService

import Servant
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

type API = GetProduct

server :: C.Context -> Server API
server context = getProduct context

type GetProduct = "product"
    :> Capture "id1" Int
    :> Capture "id2" Int
    :> Get '[JSON] (Header.Global (Maybe ResGetProduct))

getProduct' :: (MonadUnliftIO m) => C.Context -> Int -> Int -> m (Maybe ResGetProduct)
getProduct' context x y = ProductService.getProduct (getPfdyDbPool context) (getAppBridgeDbPool context) x y

getProduct :: C.Context -> Int -> Int -> Handler (Header.Global (Maybe ResGetProduct))
getProduct context x y = Header.global 1000 (\(x', y') ->
        liftIO $ getProduct' context x' y'
    ) (x, y)