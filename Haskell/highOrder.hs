import Prelude hiding (concat,map,reverse,unwords, (.), (>>))
-- 使用foldr定义函数

-- ++操作符相反的操作符
-- [1,2,3] +++ [4,5,6] => [4,5,6,1,2,3]
(+++) :: [a] -> [a] -> [a]
(+++) = foldr (:)

-- 插入排序使用foldr的写法
-- isSort [1,5,2,10,8,0,9] => [0,1,2,5,8,9,10]
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = x:y:ys
                | otherwise = y:insert x ys
isSort :: Ord a => [a] -> [a]
isSort = foldr insert []

-- 将列表中连续相同的元素去重只保留一个
-- compress [1,2,3,3,4,5,6,6,6,6,6,6,6,6,9,6,4,9,9,9,9,9] => [1,2,3,4,5,6,9,6,4,9]
skip :: Eq a => a -> [a] -> [a]
skip x [] = [x]
skip x (y:ys) | x == y = y:ys
              | otherwise = x:y:ys
compress :: Eq a => [a] -> [a]
compress = foldr skip []

-- 将元素追加到列表尾部
-- snoc 1 [2,3,4]=>[2,3,4,1]
snoc :: a -> [a] -> [a]
snoc x = foldr (:) [x]

-- 连接操作符
-- concat [[1,2,3],[4,5,6]] => [1,2,3,4,5,6]
concat :: [[a]] -> [a]
concat = foldr (++) []

-- map的另外一个实现
-- map (+1) [1,2,3] = [2,3,4]
map :: (a->b) -> [a] -> [b]
map f = foldr (\x y->f x : y) []

-- 翻转列表
-- reverse [1,2,3] => [3,2,1]
reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

-- 将字符串列表用空格连接起来
unwords :: [String] -> String
unwords [] = ""
-- 这里其实使用foldr1也是一样的
unwords xs = foldl1 (\s sl-> s++" "++sl) xs

-- 复合函数运算符的定义
(.) :: (b->c) ->(a->b) -> (a->c)
(.) f g = \x -> f (g x)

-- 定义从左到右的复合函数计算运算符
infix 9 >>
(>>) :: (a->b) -> (b->c) -> (a->c)
(>>) = flip (.)

-- 定义参数前置的复合函数运算符
(|>) :: b -> (b->c) -> c
(|>) = flip ($)

-- 库函数使用复合函数来实现
any',all' :: (a->Bool) -> [a] -> Bool
any' p = or . map p
all' p = and . map p

elem',notElem' :: Eq a => a -> [a] -> Bool
elem' = any' . (==)
notElem' = all' . (/=)