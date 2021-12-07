import Data.List

split :: Char -> String -> [String]
split d "" = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

parsepos :: String -> [Int]
parsepos s = map read $ split ',' s

grouppos :: [Int] -> [(Int, Int)]
grouppos xs = map (\x -> (head x, length x)) $ group $ sort xs

distance :: Int -> Int -> Int
distance x y = n * (n + 1) `div` 2 where n = abs (x - y)

distances :: [(Int, Int)] -> Int -> Int
distances xs n = sum $ map (\(p, c) -> c * distance n p) xs

main :: IO ()
main = do
  input <- getContents
  let pos = parsepos input
  let posfreq = grouppos pos
  let start = minimum pos
  let end = maximum pos
  print $ minimum $ map (distances posfreq) [start..end]
