import Text.Parsec

data Packet = Op Int Int [Packet] | Lit Int Int deriving (Eq, Show)

hex2bin :: String -> String
hex2bin s = s >>= hex2bin'

hex2bin' :: Char -> String
hex2bin' '0' = "0000"
hex2bin' '1' = "0001"
hex2bin' '2' = "0010"
hex2bin' '3' = "0011"
hex2bin' '4' = "0100"
hex2bin' '5' = "0101"
hex2bin' '6' = "0110"
hex2bin' '7' = "0111"
hex2bin' '8' = "1000"
hex2bin' '9' = "1001"
hex2bin' 'A' = "1010"
hex2bin' 'B' = "1011"
hex2bin' 'C' = "1100"
hex2bin' 'D' = "1101"
hex2bin' 'E' = "1110"
hex2bin' 'F' = "1111"

tonum :: Char -> Int
tonum '0' = 0
tonum '1' = 1

bin2dec :: String -> Int
bin2dec ns = foldl (\s n -> 2 * s + (tonum n)) 0 ns

p_bin :: Parsec String () Char
p_bin = oneOf "01"

p_ver :: Parsec String () String
p_ver = count 3 p_bin

p_type :: Parsec String () String
p_type = count 3 p_bin

p_group :: Parsec String () String
p_group = char '1' >> count 4 p_bin

p_lastgroup :: Parsec String () String
p_lastgroup = char '0' >> count 4 p_bin

p_type_lit :: Parsec String () String
p_type_lit = string "100"

p_type_op :: Parsec String () String
p_type_op = string "110"

p_len_t_15 :: Parsec String () Char
p_len_t_15 = char '0'

p_len_t_11 :: Parsec String () Char
p_len_t_11 = char '1'

p_len_11 :: Parsec String () String
p_len_11 = count 11 p_bin

p_len_15 :: Parsec String () String
p_len_15 = count 15 p_bin

p_lit :: Parsec String () Packet
p_lit = do
  ver <- p_ver
  p_type_lit
  gs <- many p_group
  lg <- p_lastgroup
  return (Lit (bin2dec ver) (bin2dec ((concat gs) ++ lg))) 

p_op :: Parsec String () Packet
p_op = do 
  ver <- p_ver
  typeId <- p_type
  len <- (p_len_t_15 >> p_len_15) <|> (p_len_t_11 >> p_len_11)
  ps <- many (try p_packet)
  return (Op (bin2dec ver) (bin2dec typeId) ps)

p_packet :: Parsec String () Packet
p_packet = try p_lit <|> p_op

sumVersions :: Either ParseError Packet -> Int
sumVersions (Right p) = sumVersions' p

sumVersions' :: Packet -> Int
sumVersions' (Lit v _) = v
sumVersions' (Op v _ ps) = v + (sum $ map sumVersions' ps)

main :: IO ()
main = do
  input <- getContents
  let binStr = hex2bin input
  print binStr
  let packet = parse p_packet "" binStr
  print packet
  print $ sumVersions packet
    
