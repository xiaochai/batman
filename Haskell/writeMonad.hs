import Control.Monad.Writer

left, right :: Int -> Writer String Int
left x = writer (x -1 , "move left\n")
right x = writer (x +1 , "move right\n")

move :: Int -> Writer String Int
move x = do
    a <- left x
    b <- left a
    right b

-- 归并排序，即将两个已经排好序的列表合并成一个有序的列表
-- 辅助函数，将两个有序的列表合并成一个
mergeSortList ::Ord a => [a] -> [a] -> [a]
mergeSortList [] a = a
mergeSortList a [] = a
mergeSortList (x:xs) (y:ys) | x > y = y : mergeSortList (x:xs) ys
                            | otherwise = x : mergeSortList xs (y:ys)
-- 显示多个空格缩进的函数
indent :: Int -> ShowS
indent n = showString $ replicate (4 * n) ' '
-- 显示空行
nl :: ShowS
nl = showChar '\n'
-- 归并排序，并记录过程
mergeSort :: Int -> [Int] -> Writer String [Int]
mergeSort _ [] = return []
mergeSort _ s@[_] = return s
mergeSort l s@xs = do
    tell $ (indent l.showString "mergesort: ".shows s.nl) ""
    let (ll,rl) = splitAt (length xs `div` 2) xs
    tell $ (indent (l+1).showString "merge".shows ll.shows rl.nl) ""
    liftM2 mergeSortList (mergeSort (l+2) ll) (mergeSort (l+2) rl)
    