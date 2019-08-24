import Prelude hiding ((/=),(==),not,and,or,(&&),(||))

-- 重新定义
(==),(/=),and,or,(&&),(||),xor :: Bool->Bool->Bool

(==) True True = True
(==) False False = True
(==) _ _ = False

(/=) x y | x == y = False
         | otherwise = True

not :: Bool -> Bool
not x = x == False

and True x = x
and _ _ = False

or False x = x
or _ _ = True

(&&) = and
(||) = or
xor = (/=)

infix 4 ==
infix 4 /=
infixl 3 &&
infixl 2 ||

-- 半加法器，即不含有进位的加法器
halfAdd :: Bool -> Bool -> (Bool, Bool)
halfAdd a b = (a /= b, a && b)

-- 全加法器，包含有进位
fullAdd :: Bool -> Bool -> Bool -> (Bool, Bool)
-- fullAdd a b c = (,(a/=b) /= c)
fullAdd a b c = let (s1, c1) = halfAdd a b in
                let (s2, c2) = halfAdd s1 c in (s2, c1 || c2)