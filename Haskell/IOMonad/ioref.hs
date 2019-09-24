import Data.IORef
main :: IO ()
main = do
    aRef <- newIORef 10
    modifyIORef aRef (+10)
    b <- readIORef aRef
    print b
    writeIORef aRef 100
    b <- readIORef aRef
    print b
-- 运行结果
-- ➜ runghc ioref.hs
-- 20
-- 100