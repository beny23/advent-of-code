import Data.List

parseline :: String -> [Int]
parseline = map read . map (\x -> [x])

makeBorder :: Int -> [Int]
makeBorder n = replicate n 10

addMargin :: [Int] -> [Int]
addMargin xs = 10 : xs ++ [10]

processlines :: [[Int]] -> [Int]
processlines (prev : curr : next : lines) = (processline prev curr next) ++ processlines (curr : next : lines)
processlines _ = []

processline :: [Int] -> [Int] -> [Int] -> [Int]
processline (_ : top : top' : tops) (left : curr : right : currs) (_ : bottom : bottom' : bottoms) =
  (processline' curr (top : left : right : bottom : [])) ++ processline (top : top' : tops) (curr : right : currs) (bottom : bottom' : bottoms)
processline _ _ _ = []

processline' :: Int -> [Int] -> [Int]
processline' curr others 
  | curr < minimum others = [curr]
  | otherwise             = []

main :: IO ()
main = do
  input <- getContents
  let heights = map parseline $ lines input
  let border = makeBorder $ length $ head heights
  let heightsWithBorder = border : heights ++ [border]
  let heightsWithBorderAndMargin = map addMargin heightsWithBorder
  let minHeights = processlines heightsWithBorderAndMargin
  print $ sum $ map (1+) minHeights
