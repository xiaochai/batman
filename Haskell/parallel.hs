import Control.Monad.Par
isPrime ::Integer -> Bool
isPrime 2 = True
isPrime n = n > 1 && all (\x -> (n `mod` x) /= 0)  (takeWhile (\x -> x * x < n) $ 2:[3,5..])

generateKey :: Integer -> Integer -> Integer
generateKey x y = runPar $ do
    k1 <- new
    k2 <- new
    -- 不知道为什么下面这句话会报错
    -- [k1,k2] <- sequence [new, new]
    fork $ put k1 (isPrime x)
    fork $ put k2 (isPrime y)
    p1 <- get k1
    p2 <- get k2
    return $ if p1 && p2 then x * y *10 else 0


main = print $ generateKey 2646507710984041 1066818132868207
-- main = do 
--     print $ isPrime 2646507710984041
--     print $ isPrime 1066818132868207
--     print $ 2646507710984041 * 1066818132868207 *10