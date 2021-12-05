import Data.List

split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y) where (x,y) = span (/= d) xs

parseline :: String -> ((Int, Int), (Int, Int))
parseline line = parseline' $ words line

parseline' :: [String] -> ((Int, Int), (Int, Int))
parseline' [a, "->", b] = (parsecoord a, parsecoord b)

parsecoord :: String -> (Int, Int)
parsecoord s = parsecoord' $ map read $ split ',' s

parsecoord' :: [Int] -> (Int, Int)
parsecoord' [x, y] = (x, y)

genline :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
genline ((x1, y1), (x2, y2))
  | x1 == x2                   = [(x1, y) | y <- [(min y1 y2)..(max y1 y2)]]
  | y1 == y2                   = [(x, y1) | x <- [(min x1 x2)..(max x1 x2)]]
  | abs (x1-x2) == abs (y1-y2) = [(x1 + i * signum (x2 - x1), y1 + i * signum (y2 - y1)) | i <- [0..(abs (x1-x2))]] 
  | otherwise = []

main :: IO ()
main = do
  input <- getContents
  let text = lines input
  let coords = map parseline text
  let points = concat $ map genline coords
  let summary = map length $ group $ sort points
  print $ length $ filter (>1) summary
