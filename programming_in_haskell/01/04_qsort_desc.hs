qsort_desc [] = []
qsort_desc (x:xs) = qsort_desc larger ++ [x] ++ qsort_desc smaller
    where
        larger  = [a | a <- xs, a >  x]
        smaller = [b | b <- xs, b <= x]
