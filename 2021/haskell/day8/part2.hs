import Data.List
import Data.Maybe

-- calculates how many segments overlap with a the segments of a given number and check whether that matches expectations
overlapsNumber :: [(Int, String)] -> Int -> Int -> String -> Bool
overlapsNumber numbers num overlaps s = (==overlaps) $ length $ intersect s $ fromJust $ lookup num numbers

-- checks whether a list is a given length
isLen :: Int -> [a] -> Bool
isLen len xs = len == length xs

-- parse and sort all number strings (in case any signals are in jumbled order) - this includes the output
parseAllStrings :: String -> [String]
parseAllStrings s = nub $ map sort $ filter (/="|") $ words s

-- parse and sort only output number strings (in case any signals are in jumbled order)
parseOutputs :: String -> [String]
parseOutputs s = map sort $ dropWhile (=="|") $ dropWhile (/="|") $ words s

-- create mapping for numbers with unique length (1, 4, 7, 8)
parseNumbersWithUniqLen :: [String] -> [(Int, String)]
parseNumbersWithUniqLen xs = xs >>= (\x -> maybeToList $ fmap (\n -> (n, x)) $ lookup (length x) [(2, 1), (4, 4), (3, 7), (7, 8)])

-- checks whether number already exists
exists :: [(Int, String)] -> String -> Bool
exists numbers x = all (/=x) $ map snd numbers

-- attempts to find a number by checking how many segments overlap
-- num: number to find
-- srcNum: number to base the check on
-- overlaps: how many overlaps we expect to find
-- len: expected length
-- xs: all the number strings
-- existing mappings (if a number already exists in here it is not attempted)
parseNumber :: Int -> Int -> Int -> Int -> [String] -> [(Int, String)] -> [(Int, String)]
parseNumber num srcNum overlaps len xs numbers =
   map (\y -> (num, y)) $ filter (overlapsNumber numbers srcNum overlaps) $ filter (isLen len) $ filter (exists numbers) xs

-- parses all the non-unique numbers
parseNumbers :: [String] -> [(Int, String)] -> [(Int, String)]
parseNumbers xs uniqNums = 
  foldl (\numbers f -> numbers ++ f xs numbers) uniqNums [
    parseNumber 3 7 3 5,
    parseNumber 2 4 2 5,
    parseNumber 5 4 3 5,
    parseNumber 9 3 5 6,
    parseNumber 0 1 2 6,
    parseNumber 6 0 5 6]

-- parse a line and return the numbers
parseLine :: String -> [Int]
parseLine s = 
  let strings = parseAllStrings s
      numbers = map (\(a,b) -> (b,a)) $ parseNumbers strings $ parseNumbersWithUniqLen strings 
      outputs = parseOutputs s
  in map (\x -> fromJust $ lookup x numbers) outputs

todec :: [Int] -> Int
todec ns = foldl (\s n -> 10 * s + n) 0 ns

main :: IO ()
main = do
  input <- getContents
  let allLines = lines input
  print $ sum $ map todec $ map parseLine allLines 

