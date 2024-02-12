{-# LANGUAGE TemplateHaskell #-}

module Product.Dto(
    ResGetProduct(ResGetProduct, id, name, price)
) where

import Data.Aeson
import Data.Aeson.TH

data ResGetProduct = ResGetProduct {
    id :: Int,
    name :: String,
    price :: Int
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''ResGetProduct)