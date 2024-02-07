module Common.Aop.Log(
    helloAop
) where

helloAop :: (a -> IO b) -> a -> IO b
helloAop f = \x -> do
    print "hello start"
    x' <- f x
    print "hello end"
    return x'