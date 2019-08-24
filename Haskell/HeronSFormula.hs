-- 三角形求和公式（海伦公式）
-- heronSFormula :: Float -> Float -> Float -> Float
heronSFormula :: Floating a => a-> a -> a ->a
heronSFormula a b c  = sqrt(p*(p-a)*(p-b)*(p-c)) where p = (a+b+c)/2


heronSFormula' :: Floating a => a-> a -> a ->a
heronSFormula' a b c  = let p = (a+b+c)/2 in sqrt(p*(p-a)*(p-b)*(p-c))
