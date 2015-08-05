import Data.List;

data Line = Line {
    number::Int,
    string::String
} deriving Show

sortLines :: [Line] -> [Line]
sortLines = sortBy (\x y -> (number x) `compare` (number y))

main = print $ sortLines [Line 5 "aho", Line 1 "baka"]
