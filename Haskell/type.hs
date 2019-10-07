{-# LANGUAGE GADTs #-}
import Data.List (insertBy, sortBy, permutations)
import Data.Ord (comparing)

-- 自定义bool类型
-- data为类型定义关键字，类型名称大写，这里的MyBool是个枚举类型，只有两个值
data MyBool = MyFalse | MyTrue

-- 定义星期类型，它也是个枚举类型
-- 定义中的deriving表示这个类型自动实现导入类型类中对应的函数
-- Show，所以可以在ghic中打印出对应的值，如果没有Show将会报错
-- Eq，可比较相等
-- Ord，可比较大小
-- Enum，可以使用[Mon .. Sun]来枚举生成列表，注意这里..的之前要有空格
-- Read，可以将一个字符串读取成Day类型，例如read "Mon"::Day => Mon，read "[Mon,Tue]"::[Day] => [Mon,Tue]
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq, Ord, Enum, Read)

-- 根据Day类型，定义tomorrow
tomorrow :: Day -> Day
tomorrow Mon = Tue
tomorrow Tue = Wed
tomorrow Wed = Thu
tomorrow Thu = Fri
tomorrow Fri = Sat
tomorrow Sat = Sun
tomorrow Sun = Mon

-- 由于实现了Enum类型类所以可以使用succ和pred函数
tomorrow1,yestoday :: Day -> Day
tomorrow1 Sun = Mon
tomorrow1 n = succ n
yestoday Mon = Sun
yestoday n = pred n


-- 构造类型，有点类似于C语言中的Struct，需要多个其它类型的数据来构建这个类型的数据
type Name = String
type Author = String
type ISBN = String
type Price = Float
-- 如下定义了Book类型，这个Book即是类型，也是函数，称为数据构造器，其名字并非只能与类型名一致
-- 所以这个定义也可以data Book = BookCon Name Author ISBN Price deriving (Show, Eq)
data Book = Book Name Author ISBN Price deriving (Show, Eq)
-- 属性的访问器
name :: Book -> Name
name (Book n _ _ _) = n
author :: Book -> Author
author (Book _ a _ _) = a
isbn :: Book -> ISBN
isbn (Book _ _ i _) = i
price :: Book -> Price
price (Book _ _ _ p) = p

-- 但这种属性访问器写起来繁琐，另外一种更简单的方式如下：
-- 这种写法与之前的写法等价
data Book' = Book' {
    name' :: Name,
    author' :: Author,
    isbn' :: ISBN,
    price' :: Price
}

-- 涨价
incrisePrice :: ([Book], [Book]) -> Book -> Float -> ([Book],[Book])
incrisePrice (b1,b2) b pri  = (b : b1, Book (name b) (author b) (isbn b) (price b + pri) : b2)
-- 可以使用@符号来减少冗长的表达式（其实都一样）
incrisePrice' :: ([Book], [Book]) -> Book -> Float -> ([Book],[Book])
incrisePrice' (b1,b2) b@(Book nm ath isn prc) pri  = (b : b1, Book nm ath isn (prc + pri) : b2)

-- 空列表安全的head
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- 合并两个类型不一样的列表为一个列表
disjoint :: [a] -> [b] -> [Either a b]
disjoint as bs = [Left a | a <- as] ++ [Right b| b <- bs]
-- disjoint [1,2,3,4] "abcdefg" => [Left 1,Left 2,Left 3,Left 4,Right 'a',Right 'b',Right 'c',Right 'd',Right 'e',Right 'f',Right 'g']

-- disjoint 反函数
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([],[])
partitionEithers (Left x : xs) = (x:l, r) where (l,r) = partitionEithers xs
partitionEithers (Right x : xs) = (l, x:r) where (l,r) = partitionEithers xs
-- partitionEithers [Left 1,Left 2,Left 3,Left 4,Right 'a',Right 'b',Right 'c',Right 'd',Right 'e',Right 'f',Right 'g'] => ([1,2,3,4],"abcdefg")

-- 递归定义自然数
data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

-- int转自然数
-- int2nat 10 => Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ $ int2nat $ n-1
-- 自然数转Int
-- nat2int $ Succ $ int2nat 10 => 11
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = nat2int n + 1
-- 自然数的运算
add :: Nat -> Nat -> Nat
add Zero b = b
add (Succ a) b = Succ (add a b)
-- *Main> nat2int $ add (int2nat 10) (int2nat 10) => 20

-- 结合枚举和构造方式定义的类型
-- 定义形状类型，包括圆和长方形
data Shape = Circle{
    radius::Float
}|Rect{
    width::Float,
    height::Float
}
-- 定义面积函数
area :: Shape -> Float
area (Rect a b) = a * b
area (Circle r) = pi * r * r

-- 结合枚举和递归方式定义的类型
-- 定义布尔语言
data BoolExp = TRUE | FALSE | IF BoolExp BoolExp BoolExp
-- 计算值
eval :: BoolExp -> Bool
eval TRUE = True
eval FALSE = False
eval (IF a b c) = if eval a then eval b else eval c

-- 参数化递归类型
-- 列表的定义，原生列表的定义为 data [] a = [] | a : [a]
-- 定义一个与原生类型等价的列表
data List a = Nil | Cons a (List a) deriving (Show, Eq)
lhead :: List a -> a
lhead Nil = error "nil list"
lhead (Cons x _) = x
-- 与标准列表的转化
list2myList [] = Nil
list2myList (x:xs) = Cons x (list2myList xs)
myList2list Nil = []
myList2list (Cons x xs) = x:myList2list xs

-- 同构类型
data ThreeNum = One | Two | Three
data Level = Low | Middle | High
f :: ThreeNum -> Level
f One = Low
f Two = Middle
f Three = High
g :: Level -> ThreeNum
g Low = One
g Middle = Two
g High = Three

-- 使用newtype来定义
newtype T a b = NewType (a,b)

-- 使用newtype定义的类型实现Show类型类
-- Second 1 => 1 sencod
newtype Second = Second Int
instance Show Second where 
    show (Second 0) = "0 secnod"
    show (Second 1) = "1 secnod"
    show (Second n) = show n ++ " secnods"

-- 树的定义
-- 节点和叶子都可以存储内容的树
data Tree a = Leaf a | Node a (Tree a) (Tree a)
-- 只有叶子节点可以存数据的树
data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
-- 只有节点存储数据的树
data Tree2 a = Leaf2 |Node2 a(Tree2 a) (Tree2 a)
-- 多叉树
data Tree3 a = Node3 a [Tree3 a]

-- 不存储数据的树结构
data Tree' = Leaf' | Node' Tree' Tree' deriving (Show)

-- 卡特兰树问题
-- 给定节点个数，返回所有可能的树
trees :: Int -> [Tree']
trees 0 = [Leaf']
trees n = [Node' lt rt | x<-[0..(n-1)],lt <- trees x, rt <- trees $ n-x-1]
--map length $ map trees  [0..10] => [1,1,2,5,14,42,132,429,1430,4862,16796]

-- 霍夫曼编码
-- 基本思路是组合权重最小的两个节点组成一个二叉树
data HTree a = HLeaf a | Branch (HTree a) (HTree a) deriving Show
-- 表示形式为列表，其中每一个元素为二元组，第一个元素为权重，第二为一棵霍夫曼树，但刚开始时，是一个只有一个节点的树
htree :: (Ord w, Num w) => [(w, HTree a)] -> HTree a
htree [(_, t)] = t
htree ((w1,t1):(w2,t2):wts) = htree $ insertBy (comparing fst) (w1+w2, Branch t1 t2) wts
-- 编码
serialize :: HTree a -> [(a, String)]
serialize (HLeaf a) = [(a, "")]
serialize (Branch lt rt) = [(a, '0':s)|(a, s)<-serialize lt] ++ [(a, '1':s)| (a, s)<-serialize rt]
-- huffman
huffman :: (Ord a, Ord w, Num w) => [(w, a)] -> [(a, String)]
huffman [] = []
huffman xs = let t =  [(d, HLeaf a)|(d, a) <- xs] in
    sortBy (comparing fst) $ serialize $ htree $ sortBy (comparing fst) t
-- huffman [(0.4, "A"),(0.3, "B"), (0.1, "C"), (0.1, "D"), (0.06, "E"), (0.04, "F")] => [("A","0"),("B","10"),("C","1111"),("D","110"),("E","11101"),("F","11100")]

-- 解24点
-- 定义4则运算的表达式类型
data Exp = Val Double|Plus Exp Exp|Sub Exp Exp | Mult Exp Exp | Div Exp Exp deriving (Show, Eq)
cal :: Exp -> Double
cal (Val a) = a
cal (Plus a b) = cal a + cal b
cal (Sub a b) = cal a - cal b
cal (Mult a b) = cal a * cal b
cal (Div a b) = cal a / cal b
-- 将表达式打印出来
showExp :: Exp -> String
showExp (Val a) = show a
showExp (Plus a b) = "(" ++ showExp a ++ "+" ++ showExp b ++ ")"
showExp (Sub a b) = "(" ++ showExp a ++ "-" ++ showExp b ++ ")"
showExp (Mult a b) = "(" ++ showExp a ++ "*" ++ showExp b ++ ")"
showExp (Div a b) = "(" ++ showExp a ++ "/" ++ showExp b ++ ")"
-- 将列表分成两部分，这两部分的数据可以执行四则运算，然后再对长度大于1的列表再递归划分
divide :: [a] -> [([a],[a])]
divide xs = [(take n xs, drop n xs)| n<- [1..(length xs -1)]]
-- 根据元组，拼合成四则运算
buildExpression :: ([Exp], [Exp]) -> [Exp]
buildExpression (x,y) = [op a b|a<-x, b <-y, op<- [Plus, Sub, Mult, Div]]
-- 将数据数列组合成与任意op结合的表达式
toExpression :: [Double] -> [Exp]
toExpression [] = []
toExpression [x] = [Val x]
toExpression xs = concat [buildExpression (toExpression x, toExpression y)|(x,y) <- divide xs]
-- 使用全排列函数permutations将数字全排列，再对每一个进行toExpression，取出值为24的表达式，并使用showExp展示出来
twentyfour :: [Double] -> [String]
twentyfour xs = [showExp exp |exp<- concatMap toExpression (permutations xs), cal exp == 24.0]
-- twentyfour [7,8,9,6] => ["(8.0/((9.0-7.0)/6.0))","((8.0/(9.0-7.0))*6.0)","(6.0*(8.0/(9.0-7.0)))","((6.0*8.0)/(9.0-7.0))","(8.0*(6.0/(9.0-7.0)))","((8.0*6.0)/(9.0-7.0))","(6.0/((9.0-7.0)/8.0))","((6.0/(9.0-7.0))*8.0)"]

-- 用于可以前后遍历列表的Zipper类型
data Zipper a = Zipper [a] a [a] deriving Show
-- 从数组转到Zipper类型
fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs
fromList _ = error "empty list"
-- 用于获取下一个元素和前一个元素
next,prev :: Zipper a -> Zipper a
next (Zipper ys y (x:xs)) = Zipper (y:ys) x xs
next z = z
prev (Zipper (y:ys) x xs) = Zipper ys y (x:xs)
prev z = z
-- next $ next $ fromList [1..10] => Zipper [2,1] 3 [4,5,6,7,8,9,10]

-- 基于树类型的拉锁(Zipper)
-- 树的定义在前面：data Tree a = Leaf a | Node a (Tree a) (Tree a)
data Accumulate a = Empty | R (Accumulate a) a (Tree a) | L (Accumulate a) a (Tree a)
type ZipperTree a = (Tree a, Accumulate a)
-- 三种遍历操作，左分支，右分支，上一层
left,right,up :: ZipperTree a -> ZipperTree a
left (Node a l r, accu) = (r, R accu a l) 
left a = a
right (Node a l r, accu) = (l, L accu a r)
right a = a
up (r, R accu a l) = (Node a l r, accu)
up (l, L accu a r) = (Node a l r, accu)
up z@(t, Empty) = z
-- 前面的ZipperTree是使用参数化类型来暂存，也可以使用列表来暂存
data BranchTree1 a = R1 a (Tree a) | L1 a (Tree a)
type ZipperTree1 a = (Tree a, [BranchTree1 a])
left1,right1,up1 :: ZipperTree1 a -> ZipperTree1 a
left1 (Node n l r, t) = (l, L1 n r : t)
left1 z@(Leaf a, t) = z
right1 (Node n l r, t) = (r, R1 n l : t)
right1 z@(Leaf a, t) = z
up1 (r, R1 n l:t) = (Node n l r, t)
up1 (l, L1 n r:t) = (Node n l r, t)
up1 z@(l, []) = z

-- 使用一般化的代数类型GADTs
-- 需要在文件开头添加{-# LANGUAGE GADTs #-}来打开选项，GHCi中使用:set -XGADTs
-- 定义一个具有Int、Bool、加法表达式、判等表达式的类型
data Expg a where
    ValInt :: Int -> Expg Int
    ValBool :: Bool -> Expg Bool
    Add :: Expg Int -> Expg Int -> Expg Int
    Equa :: Expg Int -> Expg Int ->Expg Bool
evalg :: Expg a -> a
evalg (ValInt a) = a
evalg (ValBool a) = a
evalg (Add a b) = evalg a + evalg b
evalg (Equa a b) = evalg a == evalg b
-- 与非GADTs的写法区别
data Exp' = ValInt' Int | ValBool' Bool | Add' Exp' Exp' | Equa' Exp' Exp'
eval' :: Exp' -> Either Int Bool
eval' (ValInt' a) = Left a
eval' (ValBool' a) = Right a
eval' (Add' a b) = case eval' a of 
    Left a -> case eval' b of
        Left b -> Left (a+b)
eval' (Equa' a b) = case eval' a of 
    Left a -> case eval' b of
        Left b -> Right (a==b)
-- eval' (Add' (ValInt' 10) (ValBool' True))表达式在编译期是不报错的，只有在运行时会报Non-exhaustive patterns
-- 而evalg (Add (ValInt 10) (ValBool True))在编译期就会报错，类型不匹配