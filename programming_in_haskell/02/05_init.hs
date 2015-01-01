myinit [] = []
myinit xs = take (length xs - 1) xs

myinit2 [] = []
myinit2 xs = reverse (tail (reverse xs))
