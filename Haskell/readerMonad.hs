import Control.Monad.Reader
import Data.List (lookup)

readLen :: Reader [a] Int
readLen = reader $ \x -> length x

readHead :: Reader [a] a
readHead = reader $ \(x:_) -> x

-- *Main> runReader readLen "abcdefg"
-- 7
-- *Main> runReader readHead "abcdefg"
-- 'a'

test :: Reader [Int] [Int]
test = do
    xs <- local (map (+1)) ask
    ys <- ask
    return ys

-- 带变量绑定的表达式
data Exp = Val Int |
           Var String |
           Add Exp Exp |
           Decl Bind Exp deriving (Show, Eq)
type Bind = (String, Int)
type Env = [Bind]

updateEnv :: Bind -> Env -> Env
updateEnv = (:)

resolve :: Exp -> Reader Env (Maybe Int)
resolve (Val i) = return (Just i)
resolve (Var s) = do
    env <- ask
    case lookup s env of
        Nothing -> return Nothing
        Just v -> return (Just v)
resolve (Add e1 e2) = do
    re1 <- resolve e1
    case re1 of
        Nothing -> return Nothing
        Just a -> do
            re2 <- resolve e2
            case re2 of
                Nothing -> return Nothing
                Just b -> return (Just (a+b))
resolve (Decl b e) = local (updateEnv b) (resolve e)

-- *Main> runReader (resolve (Decl ("x", 3) (Decl ("y", 5) (Add (Var "y") (Add (Var "x") (Var "z"))))) ) [("z", 4)]
-- Just 12