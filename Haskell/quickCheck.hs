import Test.QuickCheck

prop_even x y = even (x+y) == (even x == even y)
-- 自动生成100个测试用例，并测试通过
-- *Main> quickCheck prop_even
-- +++ OK, passed 100 tests.

prop_reverse xs ys = reverse ys ++ reverse xs == reverse (xs ++ ys)
-- *Main> quickCheck prop_reverse
-- +++ OK, passed 100 tests.

prop_reverse_twice xs = (reverse.reverse) xs == xs
-- *Main> quickCheck prop_reverse_twice 
-- +++ OK, passed 100 tests. 

-- 判断列表是否是有序
ordered :: Ord a => [a] -> Bool
ordered [] = True
ordered [_] = True
ordered (x:y:ys) = x <= y && ordered (y:ys)

-- 选择排序，即每一次遍历取出最小的一个放在开头
selectionSort :: Ord a => [a] -> [a]
-- 辅助函数，从列表中取出最小的值，并删除这个元素
selectionSortMin :: Ord a => [a] -> (a, [a])
selectionSortMin [a] = (a, [])
selectionSortMin (x:y:xs) |  x > y  = let (min, dest) = selectionSortMin(y:xs) in (min, x:dest) 
                          | otherwise = let (min, dest) = selectionSortMin(x:xs) in (min, y:dest) 
selectionSort [] = []
selectionSort xs = let (min, dest) = selectionSortMin xs in min:selectionSort dest

prop_sort xs = ordered $ selectionSort xs
-- *Main> quickCheck prop_sort
-- +++ OK, passed 100 tests.

-- 在进行排序后的列表首元素为最小元素的测试时，如果没有限定列表不为空，则会出现失败的情况
prop_headMin xs = head (selectionSort xs) == minimum xs
-- *Main> quickCheck prop_headMin
-- *** Failed! Exception: 'Prelude.head: empty list' (after 1 test):
-- []

-- 需要使用(==>)来限定测试集中不能有空列表
prop_headMin' ::Ord a => [a] -> Property
prop_headMin' xs = not (null xs) ==> head (selectionSort xs) == minimum xs
-- *Main> quickCheck prop_headMin'
-- +++ OK, passed 100 tests; 15 discarded.


newtype Exp = Exp String deriving (Show, Eq)
-- Exp = Int | Exp op Exp
instance Arbitrary Exp where
    arbitrary = do
        n <- choose (0,1) :: Gen Int
        case n of
            0 -> do
                num <- choose (0,100) :: Gen Int
                return $ Exp (show num)
            1 -> do
                op <- elements ["+", "-", "*", "/"]
                Exp exp1 <- arbitrary :: Gen Exp
                Exp exp2 <- arbitrary :: Gen Exp
                return $ Exp (exp1 ++ op ++ exp2)
-- *Main> sample (arbitrary :: Gen Exp)
-- Exp "34"
-- Exp "94"
-- Exp "15"
-- Exp "85-58*66"
-- Exp "81-3+30-83"
-- Exp "78+55/78*42+75/55+24/65*50/76*43+32*92*59*97/65/23/96*94/98/1*34-61/41*90+50+68"
-- Exp "85"
-- Exp "42+84"
-- Exp "34"
-- Exp "42"
-- Exp "96*77*24+12/18"