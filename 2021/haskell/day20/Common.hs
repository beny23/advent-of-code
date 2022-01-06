module Common where

parseLine :: String -> [Int]
parseLine = map parseChar

parseChar :: Char -> Int
parseChar '.' = 0
parseChar '#' = 1

makeBorder :: Int -> [Int]
makeBorder n = replicate n 0

addMargin :: Int -> [Int] -> [Int]
addMargin n xs =
  let border = makeBorder n
  in border ++ xs ++ border

addBorder :: Int -> [[Int]] -> [[Int]]
addBorder n xs@(x:_) =
  let topBottom = replicate n $ makeBorder $ length x
      padded = topBottom ++ xs ++ topBottom
  in map (addMargin n) padded

bin2dec :: [Int] -> Int
bin2dec ns = foldl (\s n -> 2 * s + n) 0 ns

processLines :: [Int] -> [[Int]] -> [[Int]]
processLines algo (prev : curr : next : lines) = (processLine algo prev curr next) : (processLines algo (curr : next : lines))
processLines _ _ = []

processLine :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
processLine algo (p1 : p2 : p3 : prev) (c1 : c2 : c3 : curr) (n1 : n2 : n3 : next) =
  (algo !! bin2dec [p1, p2, p3, c1, c2, c3, n1, n2, n3]) : processLine algo (p2 : p3 : prev) (c2 : c3 : curr) (n2 : n3 : next)
processLine _ _ _ _ = []