{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Product.Controller.Impl(
    getProduct
) where

import qualified Product.Dto.Response as ResProduct
import qualified Product.Service.Interface as ProductService

getProduct :: Int -> IO (Maybe ResProduct.GetProduct)
getProduct = ProductService.getProduct