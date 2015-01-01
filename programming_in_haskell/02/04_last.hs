mylast [] = []
mylast xs = drop (length xs - 1) xs

mylast2 [] = []
mylast2 xs = [head (reverse xs)]
