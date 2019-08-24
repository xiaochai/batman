import Prelude hiding (concatMap)
import Data.List (genericLength)

avg,avg1,avg2 :: (Floating a) => [a] -> a
-- avg xs = sum xs / length xs 不能这样写，因为sum和length类型不匹配
avg xs = sum xs / fromIntegral (length xs)
avg1 xs = sum xs / (fromIntegral $ length xs)
avg2 xs = sum xs / genericLength xs

-- 实现concatMap
concatMap f l = let t = map f l in concat t