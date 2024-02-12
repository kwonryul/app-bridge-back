{-# LANGUAGE DataKinds #-}

module Header(
    Global,
    global
) where

import Servant

type Global a = Headers '[Header "X-An-Int" Int] a

global :: Int -> (a -> Handler b) -> a -> Handler (Global b)
global x f = \a -> addHeader x <$> f a