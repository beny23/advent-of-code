import Data.List

split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y) where (x,y) = span (/= d) xs

todim :: [String] -> [Int]
todim xs = map read xs

dimtoribbon :: [Int] -> Int
dimtoribbon xs = 2 * (sum $ take 2 $ sort xs) + (product xs)

main :: IO ()
main = do
  input <- getContents
  let dimensions = lines input
  print $ sum $ map (dimtoribbon . todim . (split 'x')) dimensions
