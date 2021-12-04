import Data.List

split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y) where (x,y) = span (/= d) xs

todim :: [String] -> [Int]
todim xs = map read xs

dimtosqft :: [Int] -> Int
dimtosqft xs@[l, w, h] = 2*l*w + 2*w*h + 2*h*l + (product $ take 2 $ sort xs)

main :: IO ()
main = do
  input <- getContents
  let dimensions = lines input
  print $ sum $ map (dimtosqft . todim . (split 'x')) dimensions
