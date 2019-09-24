import Data.Char
import System.Environment
import System.IO
-- import qualified强制在使用这个包里的方法或者类型时，需要加上命名空间
import qualified Data.ByteString.Char8 as BS

-- 索引文件的每一个单词格式，每一个单词在索引文件中由三部分组成，单词本身以\0结尾，4字节偏移量，4字节长度
data WordIndex = WordIndex {
    word :: String,
    offset :: Int,
    expLen :: Int
} deriving (Show)

-- 使用二分查找的方式，在索引列表中搜索出对应的单词信息
searchWord :: String -> [WordIndex] -> Maybe WordIndex
searchWord _ [] = Nothing
searchWord s xs | midWord == s = Just mid
                | midWord > s = searchWord s left
                | otherwise = searchWord s right
            where (left, mid:right) = splitAt (length xs `div` 2) xs
                  midWord = map toLower (word mid)

-- 从索引文件的String中获取索引列表
getIndexList :: String -> [WordIndex]
getIndexList [] = []
getIndexList s = WordIndex  w (byteInt o) (byteInt e) : getIndexList left
    where w = takeWhile (/='\0') s
          off = drop (length w + 1) s
          o = take 4 off
          e = take 4 (drop 4 off)
          left = drop 8 off

-- 由于索引文件中4个字节表示偏移量和长度，所以需要将一个列表转化为整数
byteInt :: String -> Int
byteInt xs = foldr (\x y -> let (pos,c) = x in 2^(pos*8) * ord c + y) 0 arr 
                where arr = zip [0..] $ reverse xs

main :: IO ()
main = do
        arg <- getArgs
        n <- getProgName
        case arg of
            [] -> print $ "Usage: " ++ n ++ " <word>" -- 没有参数时，提示用法
            (a:_) -> do -- 只使用第一个参数
                -- 以二进制文件的方式打开文件，并且读取其内容，并转化为列表
                idxH <- openBinaryFile "./CET4/CET4.idx" ReadMode
                idctIdx <- hGetContents idxH
                let indexList = getIndexList idctIdx 
                let result = searchWord a indexList -- 从列表中搜索出结果来
                case result of
                    Nothing -> print $ "word:" ++ a ++ " not found!"
                    Just wrd -> do -- 接下来在释义文件中查找
                        dictH <- openFile "./CET4/CET4.dict" ReadMode
                        hSeek dictH AbsoluteSeek (toInteger $ offset wrd) -- seek到指定的位置
                        meaning <- BS.hGet dictH (expLen wrd) -- 使用ByteString里的hGet函数，获取指定字节数量的数据
                        leftStr <- hGetContents dictH
                        BS.putStrLn meaning -- 返回的ByteString也需要用到ByteString里的putStrLn函数来输出