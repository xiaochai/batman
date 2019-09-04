{-# LANGUAGE BangPatterns #-}
import Prelude hiding (gcd, product)
-- 阶乘
factorial :: Integer -> Integer
factorial n | n <= 0 = error "invalid n"
            | n == 1 = 1
            | otherwise = n * factorial (n-1)

-- 最大公约数
gcd :: Int -> Int -> Int
gcd a b | a `mod` b == 0 = b
        | otherwise = gcd b (a `mod` b)

-- 乘方
power :: Integer -> Integer -> Integer
power x n | n == 0 = 1
          | otherwise = x * power x (n-1)

-- 改进乘方
power1 :: Integer -> Integer -> Integer
power1 x n | n <= 1 = x
           | even n = let p = power1 x (n `div` 2) in p * p
           | otherwise = let p = power1 x (n `div` 2) in x * p * p

-- 列表求积
product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

-- 将元素添加到列表的末尾（与cons即(:)运算符相反）
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y:(snoc x ys)


-- 删除列表中的指定的元素
delete :: Eq a => a -> [a] -> [a]
delete x [] = [];
delete x (y:ys) = if x == y then delete x ys else y : delete x ys

-- 求列表的和
total :: Num a => [a] -> a
total [] = 0
total (x:xs) = x + total xs

-- 使用尾递归求列表的和（尾递归是指递归函数return的语句只有递归函数调用本身，而没有其它运算）。将和做为参数传给函数
total1 :: Num a => [a] -> a -> a
total1 [] a = a
total1 (x:xs) a = total1 xs (x+a)

-- 由于Haskell惰性求值的特性，之前的尾递归并不能省略相应的空间，因为x+a并不是作为结果使用，在程序中也没有进行匹配判断等，所以只有在需要的时候才求值（即最后），中间的值都会暂存于内存中
-- 需要使用!模式(bang pattern)匹配（在参数匹配前计算值）或者$!运算符（在函数调用时计算值）强制求值。
-- bang pattern需要开启才能使用，在文件开头添加{-# LANGUAGE BangPatterns #-}
-- https://downloads.haskell.org/~ghc/6.8.1/docs/html/users_guide/bang-patterns.html
-- https://ocharles.org.uk/posts/2014-12-05-bang-patterns.html
total2,total3 :: Num a => [a] -> a -> a
total2 [] a = a
total2 (x:xs) !a = total2 xs (x+a)
total3 [] a = a
total3 (x:xs) a = total3 xs $! (x+a)

-- 互调递归，即函数1调用了函数2，函数2又调用了函数1
even1,odd1 :: Int -> Bool
even1 n | n == 0 = True
       | otherwise = odd1 (n-1)
odd1 n | n == 0 = False
      | otherwise = even1 (n-1)

-- 麦卡锡91函数，它的性质是n<=100时，结构为定值91，大于 100时，为n-10
mc91 :: Int -> Int
mc91 n | n > 100 = n-10
       | otherwise = mc91 $ mc91 (n+11)

-- 斐波那契数列
fibonacci :: (Num a, Eq a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-2) + fibonacci(n-1)

-- 生成斐波那契数列列表的方法，如果使用map fibonacci [1..100]，会非常慢，因为每一次都要递归求和
-- 此函数可以直接使用列表的前两个值来计算，所以速度很快
fibs :: Int -> [Integer]
fibs 1 = [1]
fibs 2 = [1,1]
fibs n = let a = fibs (n - 1) in a ++ [sum (drop (n-3) a)]

-- fastFibs，可以快速求得某个位置的数值
fibStep :: (Integer, Integer) -> (Integer,Integer)
fibStep (u,v) = (v, u+v)

fibPair :: Int -> (Integer, Integer)
fibPair 1 = (1, 1)
fibPair n = fibStep $ fibPair (n-1)

fastFib :: Int -> Integer
fastFib n = fst $ fibPair n

-- 斐波那契数列的两个性质验证
-- 前后两项的比值接近黄金分割(sqrt 5 - 1) / 2；其实以下函数是计算列表中的前后两项值的比例
-- neighborRatio $ map fromIntegral (fibs 200)
neighborRatio :: Fractional a => [a] -> [a]
neighborRatio [n] = [0]
neighborRatio [x,y] = [0,  x /  y]
neighborRatio xs = let p = init xs in neighborRatio p ++ [ last p / last xs]

-- 连续三项中，中间项的平方与前后项的乘积相差1
lx :: [Integer] -> [Integer]
lx [x] = []
lx [x,y] = []
lx [x,y,z] = [x*z - y^2]
lx (x:y:z:xs) = (x*z - y ^ 2) : lx (y : z : xs)

-- 10进制转罗马数字
-- 找出第一个不大于n的罗马数字，然后减掉，再计算，如此往复
romeNotation :: [String]
romeNotation = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"];

romeAmount :: [Int]
romeAmount = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

conv2Rome :: Int -> String
conv2Rome 0 = ""
conv2Rome n = let combine = zip romeAmount romeNotation in 
              let (x, y) = head (dropWhile (\(x,y) -> x > n ) combine) in
              y ++ conv2Rome(n-x)

-- 二分查找
binarySearch :: (Ord a) => a -> [a] -> Bool
binarySearch a [] = False
binarySearch a xs = let (left,mid:right) = splitAt (length xs `div` 2) xs in
                    a == mid ||
                    if a > mid then  binarySearch a right
                    else binarySearch a left
-- 二分查找的另外一种写法
binarySearch1 :: (Ord a) => a -> [a] -> Bool
binarySearch1 a [] = False
binarySearch1 a xs | a > mid = binarySearch1 a right
                   | a < mid = binarySearch1 a left
                   | otherwise = True
                    where (left,mid:right) = splitAt (length xs `div` 2) xs

-- 汉诺塔（Tower of Hanoi）问题
-- (移动数量，来自哪，移到哪，借助哪) -> 操作列表
-- 使用：hanoi(10, 1, 2, 3)
hanoi :: (Int,Int,Int,Int) -> [(Int, Int)]
hanoi (1, from, to, v) = [(from, to)]
hanoi (n, from, to, v) = hanoi (n-1, from, v, to) ++ [(from, to)] ++ hanoi (n-1, v, to, from)


-- 插入排序，即往有序的列表中插入元素来达到排序的目的
-- 时间复杂度为O(n^2)，虽然可以用折半查找插入来减少比较次数，但最差复杂度仍然为O(n^2)，因为移动次数无法减少
insertSort :: Ord a => [a] -> [a]
-- 需要一个辅助函数，将元素插入已经排序好的列表中
insertSortInsert :: Ord a => a -> [a] -> [a]
insertSortInsert a [] = [a]
insertSortInsert a (x:xs) | a > x = x : insertSortInsert a xs
                        | otherwise = a:x:xs
-- 可简化成insertSort = foldr insertSortInsert
insertSort [] = []
insertSort (x:xs) = insertSortInsert x (insertSort xs)
-- 另外一种写法
insertSort1 :: Ord a => [a] -> [a] -> [a]
insertSort1 xs [] = xs
insertSort1 xs (y:ys) = insertSort1 (insertSortInsert y xs) ys

-- 冒泡排序
-- 时间复杂度为O(n^2)
bubbleSort :: Ord a => [a] -> [a]
-- 辅助函数，一趟冒泡
bubbleSortOne :: Ord a => [a] -> [a]
bubbleSortOne [] = []
bubbleSortOne [a] = [a]
bubbleSortOne (x:y:xs) | x > y = y : bubbleSortOne (x:xs)
                       | otherwise = x : bubbleSortOne (y:xs)
bubbleSort [] = []
bubbleSort xs = let p = bubbleSortOne xs in
                bubbleSort (init p) ++ [last p]
-- 另外一种使用不动点的方式，即一直调用bubbleSortOne，直到整个列表没有变化
-- 这种做法的效率没有前面那个方法高
bubbleSort1 :: Ord a => [a] -> [a]
bubbleSort1 xs = if xs == xs' then xs else bubbleSort1 xs'
                  where xs' = bubbleSortOne xs

-- 选择排序，即每一次遍历取出最小的一个放在开头
selectionSort :: Ord a => [a] -> [a]
-- 辅助函数，从列表中取出最小的值，并删除这个元素
selectionSortMin :: Ord a => [a] -> (a, [a])
selectionSortMin [a] = (a, [])
selectionSortMin (x:y:xs) |  x > y  = let (min, dest) = selectionSortMin(y:xs) in (min, x:dest) 
                          | otherwise = let (min, dest) = selectionSortMin(x:xs) in (min, y:dest) 
selectionSort [] = []
selectionSort xs = let (min, dest) = selectionSortMin xs in min:selectionSort dest

-- 快速排序，即选取一个数，以此为分割，分成小于此数的列表和大于此数的列表，再对这两个列表进行快速排序，最后合并
-- 时间复杂度是O(nlogn)
quitSort :: Ord a => [a] -> [a]
-- 辅助函数，从列表中取出大于等于a和小于a的数
quitSortSplit :: Ord a => a -> [a] -> ([a], [a])
quitSortSplit a [] = ([], [])
quitSortSplit a (x:xs) | x >= a = (min, x:max)
                       | otherwise = (x:min, max)
                        where (min, max) = quitSortSplit a xs
quitSort [] = []
quitSort (x:xs) = let (min, max) = quitSortSplit x xs in quitSort min ++ [x] ++ quitSort max
-- 另外一种写法直接使用filter函数，但是效率上比不上前一个
quitSort1 :: Ord a => [a] -> [a]
quitSort1 [] = []
quitSort1 (x:xs) = quitSort1 min ++ [x] ++ quitSort1 max
                  where min = filter (<x) xs;max = filter (>=x) xs

-- 归并排序，即将两个已经排好充的列表合并成一个有序的列表
mergeSort :: Ord a => [a] -> [a]
-- 辅助函数，将两个有序的列表合并成一个
mergeSortList ::Ord a => [a] -> [a] -> [a]
mergeSortList [] a = a
mergeSortList a [] = a
mergeSortList (x:xs) (y:ys) | x > y = y : mergeSortList (x:xs) ys
                            | otherwise = x : mergeSortList xs (y:ys)
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs = let (left,right) = splitAt (length xs `div` 2) xs in 
               mergeSortList (mergeSort left) (mergeSort right)

-- 不动点函数
fix :: (a->a)->a
fix f = f (fix f)
factorial1 :: Integer -> Integer
factorial1 = fix  (\f n -> if n == 0 then 1 else n * f (n-1))

-- 牛顿法开方
newton :: (Ord a,Fractional a) => a -> a
-- 辅助方法，需要给一个初始值
newtonHelper :: (Ord a,Fractional a) => a -> a -> a
newtonHelper c t = if abs (t' - t) < 1.0e-13 then t' else newtonHelper c t' where t' = (c/t + t)/2
newton c = newtonHelper c c

-- 牛顿法开方递归调用解法，调用方法为newton1 2 100
newton1 :: Double -> Int -> Double
newton1 c 0 = c
newton1 c n = (c/t + t)/2 where t = newton1 c (n-1)

-- 牛顿法开方，使用不动点方式
fix1 :: (t -> t -> Bool) -> (t -> t) -> t -> t
fix1 c f x | c x (f x) = x
           | otherwise = fix1 c f (f x)
newton2Helper :: Fractional a => a -> a -> a
newton2Helper c t = (c/t + t) / 2.0
newton2 :: Double -> Double
newton2 c = fix1 (\ a b -> a - b < 1.0e-13) (newton2Helper c) c

-- 比较两个数组长度，如果第一个的长度小于等于第二个，返回True，否则返回False
shorter :: [a] -> [a] -> Bool
shorter a b | la > lb = False
            | otherwise = True
            where la = length a; lb = length b

-- 使用惰性求值的思想
lazyShorter :: [a] -> [a] -> Bool
lazyShorter [] _ = True
lazyShorter _ [] = False
lazyShorter (x:xs) (y:ys) = lazyShorter xs ys
