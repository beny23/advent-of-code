import Data.List

split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y) where (x,y) = span (/= d) xs

parseages :: String -> [Int]
parseages s = map read $ split ',' s

groupages :: [Int] -> [Int]
groupages xs = map length $ group $ sort xs

nextgeneration :: [Int] -> [Int]
nextgeneration xs = nextgeneration' $ ensurelen 9 xs

nextgeneration' :: [Int] -> [Int]
nextgeneration' (p0 : p1 : p2 : p3 : p4 : p5 : p6 : p7 : p8 : []) = p1 : p2 : p3 : p4 : p5 : p6 : p7+p0 : p8 : p0 : []

ensurelen :: Int -> [Int] -> [Int]
ensurelen n xs = xs ++ replicate (n - length xs) 0

grow :: Int -> [Int] -> [Int]
grow 0 xs = xs
grow n xs = grow (n-1) $ nextgeneration xs

main :: IO ()
main = do
  input <- getContents
  let ages = groupages $ parseages input
  print $ sum $ grow 255 ages
