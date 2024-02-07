{-# LANGUAGE TemplateHaskell #-}

module Product.Dto.Response(
    GetProduct(GetProduct, id, name, price)
) where

import Data.Aeson
import Data.Aeson.TH

data GetProduct = GetProduct {
    id :: Int,
    name :: String,
    price :: Int
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''GetProduct)