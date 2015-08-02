import Data.Char

offset :: Int
offset = ord 'A' - ord 'a'

main = do cs <- getContents
          putStr $ map swapa cs

swapa :: Char -> Char
swapa c = if c == 'a' || c == 'A' then swap c else c

swap :: Char -> Char
swap c | isLower c = toUpper c
       | isUpper c = toLower c
       | otherwise = c
