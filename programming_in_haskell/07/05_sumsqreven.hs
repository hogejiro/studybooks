compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

sumsqreven = compose [sum, map (^2), filter even]
-- 動作しない理由
-- filter even は Integral a => [a] -> [a]
-- map (^2) は    Num a => [a] -> [a]
-- sum は         [a] -> Int
-- compose できるのは、引数も返し値も型が同じ関数のみ
-- sum は返し値の型が異なるため、compose できない
