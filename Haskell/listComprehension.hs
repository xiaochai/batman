import Data.Char (ord, chr, isLower)
import Data.List (delete, tails, minimumBy)
import Data.Ord (comparing)

-- a = minimumBy comparing [1,2,3,4]

-- PI的数列
-- 4 * (sum $ piSeries 1000000) == 3.1415936535887745
piSeries :: Int -> [Double]
piSeries n = [1 / (2 * fromIntegral k +1)* (-1)^k | k <- [0..n]]

-- 更快收敛的PI数列
-- sum $ piSeries1 10 == 3.1415926704506854
piSeries1 :: Int -> [Double]
piSeries1 n = [let p = 2*k+1 in 1 / fromIntegral p*(1/2)^^p * 4* (-1)^k + 1 / fromIntegral p*(1/3)^^p*4*(-1)^k | k <-[0..n]]

-- 判断某个数是否为素数
isPrime :: Integer -> Bool
factors :: Integer -> [Integer]
factors n = [x| x <- [1..n], n `mod` x == 0]
isPrime n = factors n == [1,n]

-- 生成素数列表
primes :: Integer -> [Integer]
primes n = [x | x <- [0..n], isPrime x]

-- 更快地判断是否是素数
isPrime' :: Integer -> Bool
isPrime' 2 = True
isPrime' n = n > 1 && 
            let xs = takeWhile (\x -> x * x <= n) (2:[3,5..]) in all (\x-> mod n x /= 0) xs

-- 给定一个因子，计算比它大的第一个素数
-- 这个方法还是很慢，nextPrime 24432434423421用了2秒
nextPrime :: Integer -> Integer
nextPrime n = let next = if even n then n+1 else n+2 in
              if isPrime' next then next else nextPrime next

-- 埃拉托斯特尼法(Eratosthenes sieve)生成法，即拿前面所有的素数去测试下一个数是否为素数
-- 这个方法比之前的primes快许多
sieve :: [Integer] -> [Integer]
sieve (x:xs) = x: sieve [p | p<-xs,mod p x /= 0]
primes' = sieve [2..]


-- 凯撒加密
-- 假设只对小写字母加密
encode,encode' :: String -> Int -> String
shift :: Char -> Int -> Char
shift x n = chr ((ord x - ord 'a' + n) `mod` 26 + ord 'a')
encode xs n = [if isLower x then shift x n else x | x <- xs]
encode' [] _ = []
encode' (x:xs) n | isLower x = shift x n : encode' xs n
                | otherwise = x : encode' xs n

decode,decode' :: String -> Int -> String
unshift :: Char -> Int -> Char
unshift x n = chr ((ord x - ord 'a' - n) `mod` 26 + ord 'a')
decode xs n = [if isLower x then unshift x n else x | x <-xs]
decode' [] _ = []
decode' (x:xs) n | isLower x = unshift x n : decode xs n
                | otherwise = x : decode xs n

-- 两组字母的相同程度
similarity :: [Float] -> [Float] -> Float
similarity xs ys = sum [(x-y) ^^ 2 / x| (x,y)<-zip xs ys]

-- 正常字母出现的概率
os :: [Float]
os = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]

-- 统计26个小写字母出现的次数
stat :: String -> [Int]
stat xs = let letters = ['a'..'z'] in [length [y|y<-xs, y==x] | x<-letters]

-- 计算es
es :: [Int] -> [Float]
es xs = let n = sum xs in [fromIntegral x / fromIntegral n * 100 | x <- xs ]

-- 猜测
guess :: String -> String
guessFindMin :: [(Float, String)] -> String
guessFindMin [(x, y)] = y
guessFindMin ((x1,y1):(x2,y2):xs) | x1 > x2 = guessFindMin ((x2,y2):xs)
                                  | otherwise = guessFindMin ((x1,y1):xs)
guess secret = guessFindMin m 
               where posibles = [decode secret x | x <- [0..25]]
                     rates = [es $ stat xs|xs <- posibles]
                     similaritys = [ similarity os xs|xs<-rates]
                     m = zip similaritys posibles

guess' secret = m 
               where posibles = [decode secret x | x <- [0..25]]
                     rates = [es $ stat xs|xs <- posibles]
                     similaritys = [ similarity os xs|xs<-rates]
                     m = zip similaritys posibles

-- 全排列问题
-- 思路：将列表中的每一个元素今次取出做为第一个，计算剩下数组的全排列，最后把这些全排列合并
permutationWithPre :: Eq a => [a] -> [a] ->[[a]]
permutationWithPre pre [] = [pre]
permutationWithPre pre ys = concat [permutationWithPre (pre ++ [a]) ax |(a:ax) <- tmp]
                  where tmp = [x : delete x ys|x <- ys]

permutation :: Eq a => [a] -> [[a]]
permutation = permutationWithPre []

-- 教材中更简单的方法
permutation' :: Eq a => [a] -> [[a]]
permutation' [] = [[]]
permutation' xs = [x:ys |x <- xs, ys <- permutation' (delete x xs)]

-- 插入法全排序
permutation'' :: Eq a => [a] -> [[a]]
insert :: a -> [a] -> [[a]]
insert x [] = [[x]]
insert x (y:ys) = (x:y:ys) : [ y:a|a<-insert x ys]
permutation'' [] = [[]]
permutation'' (x:xs) = concat [insert x ys |ys<- permutation'' xs]

-- 错位排列算法，即求一个1~n的列表的全排列表 ，但要排除掉第i位是i的情况
derangements :: [Int] -> [[Int]]
derangements [] = [[]]
derangements xs = [x:yx|x<-xs,x/=length xs,yx <- derangements (delete x xs)]

-- 获取一个集合的所有子集
-- 思路为，对于第一个元素和剩余的列表，所有子集为第一个元素出现与剩下列表的所有子集合并、第一个元素不出现的情况，即剩下列表的所有子集
powset :: [a] -> [[a]]
powset [] = [[]]
powset (x:xs) = concat [[x:ys,ys] | ys <- powset xs]

-- 从集合中取出n个元素
-- 思路为，取出第一个元素，如果留下，递归取剩下列表的n-1个元素，如果不留下，取剩下列表的n个元素
conbinations :: Int -> [a] -> [[a]]
conbinations 0 _ = [[]]
conbinations _ [] = []
conbinations n (x:xs) = [x:ys|ys <- conbinations (n-1) xs] ++ conbinations n xs
-- 教材中解法
conbinations' :: Int -> [a] -> [[a]]
conbinations' 0 _ = [[]]
conbinations' n xs = [y:ys | y:xs' <- tails xs, ys <- conbinations' (n-1) xs']

-- 8皇后问题
-- 用一个列表皇后的摆放位置，其中第n一个元素表示第n列，元素的值表示排放在此列的第几行，这样本身就限制了两个皇后不能摆同一列的规则
positions :: Int -> Int -> [[Int]]
positions 0 _ = [[]]
positions k n = [x:ys | x<-[1..n], ys<-positions (k-1) n]
-- 以下是限定条件
-- 1. 任意两个皇后不能在同一行
notSameRow :: [Int] -> Bool
notSameRow [] = True
notSameRow (x:xs) = notElem x xs && notSameRow xs
-- 2. 不能出现在同一对角线上
-- 不在同对角线，即行之差与列之差的绝对值不相等
notSameDiag :: [Int] -> Bool
notSameDiag [] = True
-- @符号表示第一个参数指向第二个参数，即xs指向(_:xs')
notSameDiag xs@(_:xs') = and [abs (i1-i) /= abs (p1-p)|(i,p)<-ip] && notSameDiag xs'
                        where (i1,p1):ip = zip [1..] xs
-- 8皇后问题用这个方法求解没有办法，因为8^8为16777216，即positions 8 8 生成的列表太大，无法在短时间内处理完成，我们可以看一下positions 4 4 的处理结果
queen :: Int -> [[Int]]
queen n = [xs| xs <- positions n n, notSameDiag xs, notSameRow xs]
-- *Main Data.List> queen 4
-- [[2,4,1,3],[3,1,4,2]]

-- 为了在有效时间内解决8皇后问题，我们优化一下positions方法，即在生成列表阶段就把两个限制条件给应用了
positions' :: Int -> Int -> [[Int]]
positions' 0 _ = [[]]
-- 检查生成的数和后面的数列是否满足要求
positions' k n = [x : ys | ys <- positions' (k - 1) n, x <- [1 .. n], safe x ys]
                  where safe e l  = notElem e l && all (\(dist, p) -> dist /= abs (p-e)) (zip [1..] l)
-- 使用positions' 8 8即可生成，一共是92种方式，只使用了0.02秒不到
-- 但是，如果写成positions' k n = [x : ys |  x <- [1 .. n], ys <- positions' (k - 1) n, safe x ys]，即x先生成 再生成ys的话，就还是很慢


-- 矩阵转置
-- 思路：先取第一个列，然后去掉数组的第一列，递归求转置
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose ((x:xs):xxs) = (x:[y | (y : _) <- xxs]) : transpose (xs : [ys | (_ : ys) <- xxs])

-- 矩阵乘法
-- 定义符号|*|为矩阵乘法
(|*|) :: Num a => [[a]] -> [[a]] -> [[a]]
(|*|) [] _ = []
(|*|) (x:xs) b = [sum $ zipWith (*) x y |y <- b'] : (|*|) xs b
            where b' = transpose b

-- 斐波那契数列的矩阵求法
fibMatrix :: Integer -> [[Integer]]
fibMatrix 1 = [[1,1],[1,0]]
fibMatrix n | even n = let n' = n `div` 2; m = fibMatrix n' in m |*| m
            | otherwise = let n' = (n-1) `div` 2 ; m = fibMatrix n' in m |*| m |*| fibMatrix 1

-- 图的最短路径
-- 定义一些基本结构，，Name表示地点名称，Direction表示
type Distance = Double -- 表示距离
type Name = String -- 地点名称 
type Direction = String --路径描述，如A->B->C
type Weight = (Distance, Direction) -- 距离与路径描述的二元组
type RouteMap = [[Weight]] -- 描述路径距离的矩阵

-- 将名字映射成矩阵，例如zipD ["A", "B", "C", "D"] => [["A->A","A->B","A->C","A->D"],["B->A","B->B","B->C","B->D"],["C->A","C->B","C->C","C->D"],["D->A","D->B","D->C","D->D"]]
zipD :: [Name] -> [[String]]
zipD ns = [[a ++ "->" ++ b|b <- ns ] | a <- ns]
-- 将距离数组和名字映射成的矩阵，合并
zipW :: [[Distance]] -> [Name] -> RouteMap
zipW ds ns = [zip d n | (d,n)<-zip ds $ zipD ns]
-- 两个距离相连接，注意第一个Weight的目的地和第二个的出发地要一样，例如tuplePlus (1.0, "A->B->C") (2.0, "C->D->F") => (3.0,"A->B->C->D->F")
tuplePlus :: Weight -> Weight -> Weight
tuplePlus (d1, n1) (d2, n2) = (d1+d2, n1++left)
                              where (_, left) = break (=='-') n2

-- 进行一次迭代，取出这次迭代中距离最小的
step :: RouteMap->RouteMap->RouteMap
step a b = [[minimumBy (comparing fst) $ zipWith tuplePlus ar bc | bc<-transpose b]|ar<-a]

-- 进行n次迭代的函数
iteration :: Int -> (a->a)->a->a
iteration 0 _ x = x
iteration n f x = iteration (n-1) f (f x)

-- 定义走n步的最优解
steps :: Int -> RouteMap ->RouteMap
steps n route = iteration n (step route) route

-- 不动点函数，即矩阵中的距离不再变化
fix f x = if dss == dss' then x else fix f x'
            where x' = f x 
                  dss = [map fst ds | ds <- x']
                  dss' = [map fst ds | ds <- x]

-- 最终的求最优解的函数
path :: [[Distance]] -> [Name] -> RouteMap
path dis ns = fix (step route) route
            where route = zipW dis ns

-- i表示无穷远，即不连通
i = 1/0

-- 以下是例子，定义了一个无向图
graph = [[0,1,i,i,i,i,i,1,i,i],
      [1,0,1,i,i,i,i,i,i,i],
      [i,1,0,i,i,i,i,i,1,1],
      [i,i,i,0,1,1,i,i,i,i],
      [i,i,i,1,0,i,i,i,i,i],
      [i,i,i,1,i,0,i,i,1,i],
      [i,i,i,i,i,i,0,i,1,i],
      [1,i,i,i,i,i,i,0,i,i],
      [i,i,1,i,i,1,1,i,0,i],
      [i,i,1,i,i,i,i,i,i,0]]
--  地点名称
names = ["A","B","C","D","E","F","G","H","I","J"]

-- 例子 path graph names
