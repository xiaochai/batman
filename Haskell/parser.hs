{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Data.Char
-- 由于typeclass.h已经弄坏了很多基础库的函数和类型类，所以单独起了个文件

-- 简易字符识别器
-- 定义一个新的类型，他的成员runParser是一个将字符串转成对应的类型a和剩下字符串的二元组的函数
-- 例如识别数字的Parser的runParser会将123abc识别成(123,"abc")这种
newtype Parser a = Parser{
    runParser :: String -> Maybe (a, String)
}
-- 其实现的Functor类型类，是将最终产生的结果二元组中的第一个值调用参数f函数
instance Functor Parser where
    fmap f p = Parser $ \str -> case runParser p str of
                                Nothing -> Nothing
                                Just (t, s) -> Just (f t, s)
-- 实现Applicative函数时第一个Parser的runParser运行的结果二元组一个值为a->b的函数，所以需要将这个函数应用于第二个二元组的第一个值上
instance Applicative Parser where
    pure a = Parser $ \str -> Just (a, str)
    (<*>) a b = Parser $ \str -> case runParser a str of
                                Nothing -> Nothing
                                Just (t, s) -> case runParser b s of
                                    Nothing -> Nothing
                                    Just (t2, s2) -> Just (t  t2, s2)
-- 使用第一个Parser，如果结果不为Nothing，就了这个值，否则使用第二个Parser
instance Alternative Parser where
    empty = Parser $ const Nothing
    (<|>) a b = Parser $ \str -> case runParser a str of
                                Nothing -> runParser b str
                                Just x -> Just x

-- 引入辅助函数
satisfy :: (Char->Bool) -> Parser Char
-- 这里的\case需要引入{-# LANGUAGE LambdaCase #-}，否则可使用\str -> case str of这个等价语法
satisfy f = Parser $ \case
                    [] -> Nothing
                    x:xs -> if f x then Just (x, xs) else Nothing
-- runParser (satisfy (=='a') ) "aaaaabc" => Just ('a', "aaaabc")
char :: Char -> Parser Char
char c = satisfy (==c)

-- 将数字的字符串直接转成Int
-- fmap digitToInt (satisfy isDigit) 取一个字符，转成Int，runParser (fmap digitToInt (satisfy isDigit) ) "123" => Just (1,"23")
-- many digit即取出所有的数字，组成数组runParser (many $ fmap digitToInt (satisfy isDigit) ) "123" => Just ([1,2,3],"")
-- 最后再计算这个数字列表所代表的整数
number :: Parser Int
number = fmap (foldl (\x y -> 10*x +y ) 0) (many digit)
         where digit = fmap digitToInt (satisfy isDigit)
-- runParser number "123" => Just (123, "")

sequ :: Parser a -> Parser [a] -> Parser [a]
sequ x y = Parser $ \str -> case runParser x str of
                            Nothing -> Nothing
                            Just (x1, s1) -> case runParser y s1 of
                                             Nothing -> Nothing
                                             Just (y1, s2) -> Just (x1:y1, s2)

parseStr :: String -> Parser String
parseStr strs = foldr sequ (Parser $ \str -> Just ("",str)) [char s | s <- strs]
-- (runParser (parseStr "hello")) "helloworld" => Just ("hello","world")

