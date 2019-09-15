{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
import Prelude hiding ((/=), (==),(<=),(>=),(<),(>),compare, Ordering, GT, EQ, LT,Applicative,(<*>),pure,(*>),(<*))
import Control.Applicative hiding (Applicative,(<*>),pure,liftA,liftA2,(*>),(<*),Alternative,(<|>),some,many)
import Data.Foldable
import Data.Monoid
-- 定义一个自定义的MyEq类，与标准库中的Eq类一致
class MyEq a where 
    (==),(/=) :: a -> a ->Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

-- 定义一个自定义的类型
data MyNum = O | Zero | One 

-- 实现MyEq这个类型类，因为定义中==和/=是互相定义的，所以，只要定义一个==相当于定义了/=
-- 实现类型类的语法为: instance 类型类 类型 where 函数定义 。这里的函数定义如果在类型类中已经定义了，可以省略
instance MyEq MyNum where
    O == Zero = True
    Zero == O = True
    O == O = True
    Zero == Zero = True
    One == One = True
    _ == _ = False
-- [O==Zero,Zero/=Zero,One==O, One /= Zero] => [True,False,False,True]

-- 对于参数化的类型，实现类型类时，可以使用参数限定的方式来定义
instance (MyEq m) => MyEq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
-- Just O == Just Zero => True

-- 在定义类型类时，可以加入对这个类型类的约束，也就是类型类间的依赖关系。注意类型间的依赖不能形成环！
-- 先定义Ordering类型，并实现MyEq类型类
data  Ordering  =  LT | EQ | GT
instance MyEq Ordering where
    LT == LT = True
    EQ == EQ = True
    GT == GT = True
    _ == _ = False
-- 定义依赖于MyEq类型类的MyOrd类型类，这里的定义与Prelude里的Ord定义基本一致
class (MyEq a) => MyOrd a where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a
    compare x y
         | x == y    =  EQ
         | x <= y    =  LT
         | otherwise =  GT
    x <= y           =  compare x y /= GT
    x <  y           =  compare x y == LT
    x >= y           =  compare x y /= LT
    x >  y           =  compare x y == GT
    max x y 
         | x <= y    =  y
         | otherwise =  x
    min x y
         | x <= y    =  x
         | otherwise =  y
-- 以上可以看出<= 与compare是互相定义的，所以实现时，只要定义一个即可

-- Functor函子类型类
-- 他的做用是将容器里的值通过一个函数映射成另外一个值
-- 这里的参数f为容器类型的构造器，例如列表类型为[]
-- 注意这里实现的类型必须是一个容器，即kind必须为*->*才行，像Either这种*->*->*的就无法实现Functor
class MyFunctor f where 
    fmap' :: (a->b) -> f a -> f b

-- 将Maybe实现为Functor类型类
instance MyFunctor Maybe where
    fmap' func (Just x) = Just (func x) 
    fmap' func Nothing = Nothing
-- fmap' (+190) (Just 10) => 200

-- 如果硬要将*->*->*实现为函子类型类，则需要提供一个参数出来
-- 例如函数类型(->)是一个*->*->*的类型，则我们可以给出其输入类型(->) r，使其变成*->*类型，这样就可以实现Functor了
instance MyFunctor ((->) r) where
    fmap' f g = (\x -> f (g x))
-- 以上这个fmap'通过简化为 f $ g或者 f.g，最终fmap' = .，即复合函数
-- fmap' (+(100::Int)) (+ (20::Int))这就将+20这个函数映射成了\x -> (x+20) + 100

-- 有问题的函子类型类实现
data Container a = Container a Int deriving Show
instance MyFunctor Container where
    fmap' f (Container x i) = Container (f x) (i+1)
-- fmap' id  (Container "abc" 1) => Container "abc" 2
-- fmap' (showChar 'a' . showChar 'c')  (Container "abc" 1) => Container "acabc" 2
-- (fmap' (showChar 'a') . fmap' (showChar 'c')) (Container "abc" 1) => Container "acabc" 3

-- Applicative类型类
class (Functor f) => Applicative f where 
    pure :: a -> f a
    (<*>) :: f (a->b) -> f a -> f b
    infixl 4 <*>

instance Applicative Maybe where
    pure = Just
    (<*>) Nothing _ = Nothing
    (<*>) (Just f) (Just a) = Just (f a)
    -- (<*>) (Just f) arg = fmap f arg
-- (Just (+10)) <*> Just 2  => Just 12
-- Just (+) <*> Just 2 <*> Just 10 => Just 12

-- liftA定义方便的处理函数来处理(+) <$> Just 5 <*> Just 10这种类型的
liftA :: Applicative f => (a->b) -> f a -> f b
-- liftB定义方便的函数来处理pure (+) <*> Just 5 <*> Just 10这种类型的
liftA2 :: Applicative f => (a->b->c) -> f a -> f b -> f c
liftA f a  = pure f <*> a
liftA2 f a b = pure f <*> a <*> b
-- liftA (+) (Just 5) <*> Just 10 => Just 15
-- liftA2 (+) (Just 5) (Just 10) => Just 15

-- *>运算符，返回两个参数的后一个
(*>) :: Applicative f => f a -> f b ->f b
(*>) u v = pure (const id) <*> u <*> v
-- Just 1 *> Just 2 => Just 2
-- <*运算符，返回两个参数的第一个
(<*) :: Applicative f => f a -> f b ->f a
(<*) u v = pure const <*> u <*> v
-- Just 1 *> Just 2 => Just 1

-- Alternative 类型类
-- 此类型类的作用是选取合理的值
class (Applicative f) => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
-- Maybe类型实现Alternative类型类
instance Alternative Maybe where 
    empty = Nothing
    (<|>) Nothing x = x
    (<|>) (Just x) _ = Just x
-- Nothing <|> Just 1 <|> Just 7 => Just 1

-- (take 5) <$> ( some (Just 5))不为知道啥这个式子无法使用惰性求值算出结果
some,many :: (Alternative f) => f a -> f [a]
some v = some_v
         where many_v = some_v <|> pure []
               some_v = (:) <$> v <*> many_v
many v = many_v
         where many_v = some_v <|> pure []
               some_v = (:) <$> v <*> many_v

-- 定义一个树类型，并实现Foldable类型类
data Tree a = Leaf a | Node (Tree a) a (Tree a)
instance Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Node l n r) = foldMap f l `mappend` f n `mappend` foldMap f r
-- 压平树
flatten :: Tree a-> [a]
flatten t = foldMap (\x -> [x]) t
-- 经过Eta简化之后 变成flatten = foldMap (\x -> [x])，另外(\x -> [x])与(:[]等价)，所以flatten = foldMap (:[])
--  foldMap (:[]) ( Node (Leaf True) False (Leaf False))=>[True,False,False]
-- 树求和
sumTree :: (Num a) => Tree a -> Sum a
sumTree = foldMap Sum
-- sumTree ( Node (Leaf 3) 2 (Leaf 1)) => Sum {getSum = 6}

-- 多参数的类型类
-- 需要{-# LANGUAGE MultiParamTypeClasses #-}选项
class GEq a b where
    equals :: a -> b -> Bool
data Nat = Zero' | Succ Nat deriving (Show)
-- 定义一个Nat类型和列表类型的GEq实现，比较列表的长度是否与Nat相等
instance GEq Nat [a] where
    equals = eq
        where eq Zero' [] = True
              eq (Succ n) (_:xs) = eq n xs
              eq _ _ = False
-- equals (Succ (Succ Zero')) [1,2] => True

-- 以下例子因为产生的类型冲突会报错，所以注释掉
-- -- 多个参数的类型类产生的歧义
-- class Func a b where
--     fun :: a -> b
-- instance Func Int Nat where
--     fun _ = Zero'
-- instance Func Int Int where
--     fun _ = 0
-- -- 此时使用fun (1::Int)就会报错，因为编译器不知道返回的类型是Nat还是Int
-- -- 但可以指定返回值的类型来防止歧义(fun (1::Int))::Nat => Zero'

-- 多参数强制确定时的写法
-- 需要开启{-# LANGUAGE FunctionalDependencies #-}
class Func a b | a->b where
    fun :: a -> b
instance Func Int Nat where
    fun _ = Zero'
-- 因为已经确定了类型Int必须是与Nat对应，所以以下的语句会报Functional dependencies conflict between instance declarations错
-- instance Func Int Int where
--     fun _ = 0

-- 多个参数决定
class (Num a, Num b, Num c) => GPlus a b c | a b -> c where
    plus :: a -> b -> c
instance GPlus Int Int Float where
    plus a b = fromIntegral a + fromIntegral b
instance GPlus Int Double Double where
    plus a b = fromIntegral a + b

-- 另外一个歧义的例子
-- 如果没有ce -> e的限定，这里的empty1是通不能编译的
class Collection e ce | ce -> e, e -> ce where
    empty1 :: ce
    insert :: e -> ce -> ce
    member :: e -> ce -> Bool
-- 定义一个实现
-- 由于a 和 [a]有依赖关系，所以需要打开{-# LANGUAGE FlexibleInstances #-}
instance Eq a => Collection a [a] where
    empty1 = []
    insert x xs = x:xs
    member = elem
-- 如果没有e->ce的限定，那么insert 'c' empty1会报错

-- 使用关联类型
class (Num a, Num b) => GPlus1 a b where
    type SumType a b :: *
    plus1 :: a -> b -> SumType a b
instance GPlus1 Int Int where
    type SumType Int Int = Float
    plus1 a b = fromIntegral a + fromIntegral b
instance GPlus1 Int Double where
    type SumType Int Double = Double
    plus1 a b = fromIntegral a + b

-- 定长列表
data O = VO
newtype S n = VS n
-- 开启GADTs
data FList n t where
    FNil :: FList O t
    FCons :: t -> FList n t -> FList (S n) t

-- 定长数组的编译器检查
class FIndex m n where
    fIndex :: m -> FList n t -> t
instance FIndex O (S n) where
    fIndex VO (FCons x _) = x
instance FIndex m n => FIndex (S m) (S n) where
    fIndex (VS m) (FCons _ xs) = fIndex m xs
-- fIndex (VS (VS (VS  VO))) $ FCons 'a' (FCons 'b' (FCons 'c' (FCons 'd' FNil))) => 'd'
-- fIndex (VS (VS (VS  VO))) $ FCons 'a' (FCons 'b' (FCons 'c'  FNil))会报错

-- 运行时重载
-- 定义矩形和圆形
data Rect = Rect Double Double deriving (Show)
newtype Circle = Circle Double deriving (Show)
class HasArea t where
    area :: t -> Double
instance HasArea Rect where
    area (Rect a b) = a * b
-- area (Rect 10 2) => 20.0
instance HasArea Circle where
    area (Circle r) = pi * r * r
--  area (Circle 1) => 3.141592653589793
-- 使用枚举类型的方式将不同形状加入到列表中，但扩展性差
data Shape = ShapeRect Rect|ShapeCircle Circle deriving (Show)
-- [ShapeRect (Rect 10 10),ShapeCircle (Circle 10)]

data Shape' where
    Shape' :: HasArea t => t -> Shape' 
-- length [Shape' (Rect 1 2), Shape' (Circle 1)] => 2
-- 实现HasArea类型类，这样好计算面积
instance HasArea Shape' where
    area (Shape' shape) = area shape
-- map area  [Shape' (Rect 1 2), Shape' (Circle 1)] => [2.0,3.141592653589793]
-- 另外一种写法
-- 使用此式子时，需要{-# LANGUAGE ExistentialQuantification #-}选项，这看上去与已经被淘汰的{-# LANGUAGE DatatypeContexts #-}，都是给参数添加限定
data Shape1 = forall a. (HasArea a) => Shape1 a
-- newtype (HasArea a) => Shape1 a =  Shape1 a

-- 可变长参数的加法
class Addition t where
    add :: Int -> t
instance Addition Int where
    add t = t
instance (Addition t) => Addition (Int -> t) where
    add i x = add (x + i)
   