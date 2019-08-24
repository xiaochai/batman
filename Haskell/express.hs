-- if then else
isTwo :: Int -> Bool
isTwo x = if x==2 then True else False
-- isTwo x = x==2
-- isTwo = \x-> x==2

-- case of
daysOfMonth :: Int -> Int
daysOfMonth x = case x of
    1 -> 31
    2 -> 28
    3 -> 31
    4 -> 30
    _ -> error "invalid month"

daysOfMonth' :: Int -> Int
daysOfMonth' 1 = 31
daysOfMonth' 2 = 28
daysOfMonth' 3 = 31 
daysOfMonth' 4 = 30
daysOfMonth' _ = error "invalid month"

myAbs :: (Num a, Ord a) => a -> a
myAbs n | n > 0 = n
        | otherwise = -n


head' :: [a] -> a
head' [] = error "empty"
head' (x:_) = x 

-- defind <=>
infix 5 <=>
(<=>) :: Ord a => a -> a -> Int
(<=>) x y | x > y = 1
          | x < y = -1
          | otherwise = 0