import Data.Char

type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Bit
parity bits | odd $ sum bits = 1
            | otherwise      = 0

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip (iterate (*2) 1) bits]

addparity :: [Bit] -> [Bit]
addparity bits = (parity bits) : bits

encode :: String -> [Bit]
encode = concat . map (addparity . make8 . int2bin . ord)

checkparity :: [Bit] -> [Bit]
checkparity (bit:bits) | even $ sum (bit:bits) = bits
                       | otherwise             = error "parity error"

decode :: [Bit] -> String
decode = map (chr . bin2int . checkparity) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- main = putStrLn $ show $ transmit "test"
-- test

error_transmit :: String -> String
error_transmit = decode . error_channel . encode

error_channel :: [Bit] -> [Bit]
error_channel = tail

-- main = putStrLn $ show $ error_transmit "test"
-- 08_09_notice_error_by_parity_bit.hs: parity error

