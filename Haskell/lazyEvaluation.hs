newtype N = N Int
data D = D Int

n (N i) = 42
d (D i) = 42

-- *Main> n undefined
-- 42
-- *Main> d undefined
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:78:14 in base:GHC.Err
--   undefined, called at <interactive>:3:3 in interactive:Ghci2

-- 惰性求值对计算结果的影响
data D2 = D2 !Int
d2 (D2 i) = 42

-- *Main> d2 (D2 undefined)
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:78:14 in base:GHC.Err
--   undefined, called at <interactive>:14:8 in interactive:Ghci1
-- *Main> d (D undefined)
-- 42