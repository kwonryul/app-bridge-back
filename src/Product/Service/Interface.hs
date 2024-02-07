module Product.Service.Interface(
    getProduct
) where

import Common.Aop.Log
import qualified Product.Dto.Response as ResProduct
import qualified Product.Service.Impl as Impl

getProduct :: Int -> IO (Maybe ResProduct.GetProduct)
getProduct = helloAop Impl.getProduct