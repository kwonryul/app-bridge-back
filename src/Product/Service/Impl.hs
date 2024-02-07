module Product.Service.Impl(
    getProduct
) where

import qualified Product.Dto.Response as ResProduct

getProduct :: Int -> IO (Maybe ResProduct.GetProduct)
getProduct x =
    if x == 35 then
        return $ Just (ResProduct.GetProduct 35 "kwonryul" 3000)
    else
        return Nothing