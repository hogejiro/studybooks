-- mult 3 4
-- = { applying mult }
-- (\x -> (\y -> x * y)) 3 4
-- = { applying \x -> (\y -> x * y) }
-- (\y -> 3 * y) 4
-- = { applying \y -> 3 * y }
-- 3 * 4
-- = { applying * }
-- 12
