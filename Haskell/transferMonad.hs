{-# LANGUAGE LambdaCase #-}

import Control.Monad.State
import Control.Monad.Trans.Maybe
-- import Data.Functor.Identity
-- import Control.Monad.Trans.Identity

-- 自己构造的Identity的Monad转化器
newtype IdentityT m a = IdentityT { runIdentityT :: m a }
-- 要将IdentityT m实现Monad类型类，根据Monad类型类的实现
-- class  Monad m  where
--     (>>=)  :: m a -> (a -> m b) -> m b
--     return :: a -> m a
-- 此时的IdentityT m实现Monad类型类的(>>=)和return类型为：
-- return :: a -> IdentityT m a
-- (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
instance (Monad m) => Monad (IdentityT m) where
    return a = IdentityT $ return a
    itm >>= k = IdentityT $ do
        a <- runIdentityT itm -- runIdentityT itm得到的类型为IdentityT m a中的m，所以a的类型就是a
        runIdentityT (k a) -- k a的类型为IdentityT m b，所以这个do语句返回的类型为m b, 而最终>>=需要返回 IdentityT m b，所以最外层需要加一个IdentityT构造函数

-- 这两个Applicative和Functor实现必不可少，不然会报如下错误：
-- • Could not deduce (Applicative (IdentityT m))
--     arising from the superclasses of an instance declaration
instance (Monad m) => Applicative (IdentityT m) where
            pure = return
            (<*>) = ap 
instance (Monad m) => Functor (IdentityT m) where
            fmap = liftM

-- 使用IdentityT的例子
imi :: IdentityT Maybe Int
imi = IdentityT $ Just 10
imi2 :: IdentityT Maybe Int
imi2 = (>>=) imi $  \x -> IdentityT $ Just (x+10)

-- 使用StateT和Maybe来处理push和pop
-- newtype StateT s (m :: Type -> Type) a
-- 构造函数为：StateT (s -> m (a, s))
pushSM :: Int -> StateT [Int] Maybe ()
pushSM n = StateT $ \xs -> Just ((), n:xs)

popSM :: StateT [Int] Maybe Int
popSM = StateT $ \case
    [] -> Nothing
    x:xs -> Just (x, xs)
-- *Main> runStateT popSM []
-- Nothing

-- 使用MaybeT和State来处理push和pop
-- newtype MaybeT m a
-- MaybeT 
--     runMaybeT :: m (Maybe a)
pushMS :: Int -> MaybeT (State [Int]) ()
pushMS n = MaybeT $ state $ \xs -> (Just (), n:xs)

popMS :: MaybeT (State [Int]) Int
popMS = MaybeT $ state $ \case
    [] -> (Nothing,  [])
    x:xs -> (Just x, xs)
-- *Main> runState (runMaybeT popMS  ) []
-- (Nothing,[])

-- 使用State和Maybe Int组合来处理问题
pushSM' :: Int -> State [Int] (Maybe ())
pushSM' n = state $ \xs -> (Just (), n:xs)
popSM' :: State [Int] (Maybe Int)
popSM' = state $ \case
            [] -> (Nothing, [])
            x:xs -> (Just x, xs)
-- *Main> runState popSM' []
-- (Nothing,[])

-- 比较组合与转换器之间在处理上的区别
-- 可以看出由于转换器在do中直接取出的类型就是Int，所以不需要进行一次模式匹配
tSM' :: State [Int] (Maybe Int)
tSM' = do
    pushSM' 5
    a <- popSM'
    case a of
        Nothing -> return Nothing
        Just x -> return $ Just (x+1)

tMS :: MaybeT (State [Int]) Int
tMS = do
    pushMS 5
    a <- popMS
    return (a+1)
