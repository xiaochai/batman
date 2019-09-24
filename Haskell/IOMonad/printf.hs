{-# LANGUAGE FlexibleInstances #-}
-- 在开始前我们先定义一个函数，他的作用其实是最简单的printf 只支持一个%s的情况
myFormat :: Show a => String -> a -> String
myFormat ('%':'s':xs) s = show s ++ xs
myFormat (c:cs) s = c : myFormat cs s
myFormat "" _ = ""
-- myFormat (myFormat "I'm %s, %s years old!" "Lee") 15  => "I'm \"Lee\", 15 years old!"

-- printf可以是以下的这些形式
-- printf :: String -> IO ()
-- printf :: Show u => String -> u -> IO ()
-- printf :: Show u => String -> u -> u -> IO ()
-- 抽离公共的部分，可以定义一个类型类，其方法为String -> t
class MyPrintf t where
    myPrintf :: String -> t
-- 以下是第一种情况，String -> IO()的情况，此时的类型即为IO ()
-- 这里必须添加FlexibleInstances才不会报错，因为默认情况下 type class 实例不支持IO ()
instance MyPrintf (IO ()) where
    myPrintf = putStrLn
-- 另外其它情况下，可以把Show u -> MyPrintf t实现成MyPrintf，这样 u -> IO() 是MyPrintf ， u -> u -> IO() 也是MyPrintf
-- 这样就把所有的printf场景都抽象成了MyPrintf类型类，可以调用myPrintf来求值
instance (Show u, MyPrintf t) => MyPrintf (u -> t) where
    -- 由于t的类型是u->t，所以为了返回t，我们在myPrintf又加了一个参数，其中s可以理解成formatstring，而u为参数，调用myFormat s u，即将u应用到s中生成一个字符串
    -- 但参数可能还没有应用完全，所以还需要对这个表达式调用myFormat方法，直到走到IO()的基本情形
    myPrintf s u = myPrintf (myFormat s u)