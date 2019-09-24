import System.Environment
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "empty args"
        _ -> do 
            x <- getProgName
            print $ "Program name:" ++ x
            print $ "Args:" ++ concatMap (++ ",") args
