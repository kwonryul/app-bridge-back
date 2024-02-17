{-# LANGUAGE OverloadedStrings #-}

module Middleware(
    corsMiddleware
) where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Method

corsMiddleware :: Middleware
corsMiddleware = cors (\_ -> Just $ simpleCorsResourcePolicy {
      corsOrigins = Just (["http://localhost:3000", "http://localhost:8080", "null"], True)
    , corsMethods = [methodGet, methodPost, methodPatch, methodDelete, methodHead]
    , corsMaxAge = Just 3600
    , corsVaryOrigin = True
    , corsRequireOrigin = True
    , corsIgnoreFailures = False
    })