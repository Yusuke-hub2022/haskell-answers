import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\ x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit] 
make8 bits = take 8 (bits ++ repeat 0)

-- Add
parity :: [Int] -> Bit
parity bits = l `mod` 2
              where l = length (filter odd bits)

-- Add
addprty :: [Bit] -> [Bit]
addprty bits = (parity bits) : bits

encode :: String -> [Bit]
encode = concat . map (addprty . make8 . int2bin . ord) -- Change

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- Add
chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

-- Add
pcheck :: [Bit] -> [Bit]
pcheck bits | head bits == p = tail bits
               | otherwise      = error "Parity Error"
               where p = parity (tail bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . pcheck) . chop9 -- Change

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = tail -- Change (07-08)
