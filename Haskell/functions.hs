-- add :: Int a => a -> a -> a
-- sub :: Num a => a -> a -> a
add :: Num a => a -> a -> a
add a b  = a+b
add' :: Int -> Int -> Int
add' a b  = a+b

sub :: (Show a, Num a) => a -> a -> a
sub a b  = a - b

mul :: Show a => Num a => a -> a -> a
mul a b = a * b

and :: (Eq a) => a -> a -> Bool
and = \x -> \y -> x == y

not :: (Eq a) => a -> a -> Bool
not = \x y -> x /= y