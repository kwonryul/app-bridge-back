{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Product.Controller.Interface(
    API,
    server
) where

import Common.Aop.Log
import qualified Product.Dto.Response as ResProduct
import qualified Product.Controller.Impl as Impl

import Servant
import Control.Monad.IO.Class

type API = "product" :> Capture "id" Int :> Get '[JSON] (Maybe ResProduct.GetProduct)

server :: Server API
server = liftIO . getProduct

getProduct :: Int -> IO (Maybe ResProduct.GetProduct)
getProduct = helloAop Impl.getProduct