-- 四则运算的表达式类型
data Exp = Lit Integer | Add Exp Exp | Sub Exp Exp | Mul Exp Exp | Div Exp Exp
eval :: Exp -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2
-- eval (Mul  (Lit 10) (Lit 20)) => 200

-- 处理除以0的情况
safeEval :: Exp -> Maybe Integer
safeEval (Lit n) = Just n
safeEval (Add e1 e2) = case safeEval e1 of
    Nothing -> Nothing
    Just x -> case safeEval e2 of
        Nothing -> Nothing
        Just y -> Just (x + y)

safeEval (Sub e1 e2) = case safeEval e1 of
    Nothing -> Nothing
    Just x -> case safeEval e2 of
        Nothing -> Nothing
        Just y -> Just (x - y)

safeEval (Mul e1 e2) = case safeEval e1 of
    Nothing -> Nothing
    Just x -> case safeEval e2 of
        Nothing -> Nothing
        Just y -> Just (x * y)

safeEval (Div e1 e2) = case safeEval e1 of
    Nothing -> Nothing
    Just x -> case safeEval e2 of
        Nothing -> Nothing
        Just 0 -> Nothing
        Just y -> Just (x `div` y)
-- safeEval (Div  (Lit 10) (Lit 0)) => Nothing

-- 抽离公共部分
evalSeq :: Maybe Integer -> (Integer -> Maybe Integer) -> Maybe Integer
evalSeq mi f = case mi of 
    Nothing -> Nothing
    Just a -> f a
-- evalSeq (Just 10) $ \x -> Just (x *2) => 20

-- 借助evalSeq，实现四则运算
safeEval1 :: Exp -> Maybe Integer
safeEval1 (Lit n) = Just n
safeEval1 (Add e1 e2) = evalSeq (safeEval1 e1) $ \x -> evalSeq (safeEval1 e2) (\y -> Just (x+y))
safeEval1 (Sub e1 e2) = evalSeq (safeEval1 e1) $ \x -> evalSeq (safeEval1 e2) (\y -> Just (x-y))
safeEval1 (Mul e1 e2) = evalSeq (safeEval1 e1) $ \x -> evalSeq (safeEval1 e2) (\y -> Just (x*y))
safeEval1 (Div e1 e2) = evalSeq (safeEval1 e1) $ \x -> evalSeq (safeEval1 e2) (\y -> if y == 0 then Nothing else Just (x `div` y))
-- safeEval1 (Div  (Lit 10) (Lit 5)) => Just 2

-- 借助Maybe Monad来实现四则运算
safeEval2 :: Exp -> Maybe Integer
safeEval2 (Lit n) = return n
safeEval2 (Add e1 e2) = safeEval2 e1 >>= \x -> safeEval2 e2 >>= (\y -> return (x + y))
safeEval2 (Sub e1 e2) = safeEval2 e1 >>= \x -> safeEval2 e2 >>= (\y -> return (x - y))
safeEval2 (Mul e1 e2) = safeEval2 e1 >>= \x -> safeEval2 e2 >>= (\y -> return (x * y))
safeEval2 (Div e1 e2) = safeEval2 e1 >>= \x -> safeEval2 e2 >>= (\y -> if y == 0 then Nothing else return (x `div` y))

-- 使用do <-语法来实现
safeEval3 :: Exp -> Maybe Integer
safeEval3 (Lit n) = return n
safeEval3 (Add e1 e2) = do { x <- safeEval3 e1; y <- safeEval3 e2 ;return (x + y) }



