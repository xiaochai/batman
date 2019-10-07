import Control.Monad.State

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)
-- 不使用状态Monad时的程序
labelTree :: Tree a -> Tree (a, Int)
labelTree t = fst $ ntAux t 0

ntAux :: Tree a -> Int -> (Tree (a,Int), Int)
ntAux (Leaf a) n = (Leaf (a, n), n+1)
ntAux (Node l a r) n = let (lt, n') = ntAux l (n+1) in
                       let (rt, n'') = ntAux r n' in
                       (Node lt (a, n) rt, n'')

-- 使用状态Monad来处理
labelTree' :: Tree a -> Tree (a, Int)
labelTree' t = evalState (ntAux' t) 0
increase :: State Int Int
increase = state $ \i -> (i, i+1)
ntAux' :: Tree a -> State Int (Tree (a, Int))
ntAux' (Leaf a) = do
    n1 <- increase
    return (Leaf (a, n1))
ntAux' (Node l n r) = do
    n1 <- increase
    lt <- ntAux' l
    rt <- ntAux' r
    return (Node lt (n, n1) rt)

-- *Main> labelTree' $ Node (Node (Leaf 5) 3 (Leaf 2)) 7 (Leaf 9)
-- Node (Node (Leaf (5,2)) (3,1) (Leaf (2,3))) (7,0) (Leaf (9,4))
-- *Main> labelTree $ Node (Node (Leaf 5) 3 (Leaf 2)) 7 (Leaf 9)
-- Node (Node (Leaf (5,2)) (3,1) (Leaf (2,3))) (7,0) (Leaf (9,4))

-- 使用状态Monad来实现栈结构
type Stack = [Int]
pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)
push :: Int -> State Stack ()
push n = state $ \xs -> ((), n:xs)
peek :: State Stack Int
peek = state $ \(x:xs) -> (x, x:xs)

test :: State Stack Int
test = do
    push 1
    push 2
    push 3
    a1 <- pop
    a2 <- pop
    return (a1 + a2)
-- *Main> runState test []
-- (5,[1])
