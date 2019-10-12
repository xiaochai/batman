import Control.Concurrent
import Control.Monad
import System.IO
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    _ <- forkIO $ replicateM_ 20 (putStrLn "thread 1")
    _ <- forkIO $ replicateM_ 20 (putStrLn "thread 2")
    return ()