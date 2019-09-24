main :: IO()
main = do
    putStr "What's your name?"
    name <- getLine
    putStrLn $ "Hello " ++ name