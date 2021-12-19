import Text.Parsec
import Text.Parsec.Pos
import Control.Monad

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

p_position :: Int -> Parsec String () [a]
p_position n = do
  pos <- getPosition
  guard $ (sourceColumn pos) >= n
  return []

p_lit :: Parsec String () Packet
p_lit = do
  ver <- p_ver
  p_type_lit
  gs <- many p_group
  lg <- p_lastgroup
  return (Lit (bin2dec ver) (bin2dec ((concat gs) ++ lg))) 

p_op_by_len :: Parsec String () [Packet]
p_op_by_len = do
  len <- try p_len_t_15 >> p_len_15
  pos <- getPosition
  let nextN = (sourceColumn pos) + (bin2dec len)
  ps <- manyTill p_packet $ p_position nextN
  return ps

p_op_by_num :: Parsec String () [Packet]
p_op_by_num = do
  len <- try p_len_t_11 >> p_len_11
  ps <- count (bin2dec len) p_packet
  return ps

p_op :: Parsec String () Packet
p_op = do 
  ver <- p_ver
  typeId <- p_type
  ps <- p_op_by_len <|> p_op_by_num
  return (Op (bin2dec ver) (bin2dec typeId) ps)

p_packet :: Parsec String () Packet
p_packet = try p_lit <|> p_op

evaluate' :: Either ParseError Packet -> Int
evaluate' (Right p) = evaluate p

evaluate :: Packet -> Int
evaluate (Lit _ n) = n
evaluate (Op _ 0 ps) = evaluateAll sum ps
evaluate (Op _ 1 ps) = evaluateAll product ps
evaluate (Op _ 2 ps) = evaluateAll minimum ps
evaluate (Op _ 3 ps) = evaluateAll maximum ps
evaluate (Op _ 5 ps) = evaluateAll (boolOp (>)) ps
evaluate (Op _ 6 ps) = evaluateAll (boolOp (<)) ps
evaluate (Op _ 7 ps) = evaluateAll (boolOp (==)) ps

evaluateAll :: ([Int] -> Int) -> [Packet] -> Int
evaluateAll f ps = f $ map evaluate ps

boolOp :: (Int -> Int -> Bool) -> [Int] -> Int
boolOp f xs = bool2num $ f (head xs) (head $ tail xs)

bool2num :: Bool -> Int
bool2num True = 1
bool2num False = 0

main :: IO ()
main = do
  input <- getContents
  let binStr = hex2bin input
  print binStr
  let packet = parse p_packet "" binStr
  print packet
  print $ evaluate' packet
    
