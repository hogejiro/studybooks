mymult :: Num a => a -> a -> a -> a
mymult = \x -> (\y -> (\z -> x * y * z))
