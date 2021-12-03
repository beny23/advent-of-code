slidingWindow :: Int -> [Int] -> [[Int]]
slidingWindow n xs = filter (\xs -> length xs == n) $ slidingWindow' n xs

slidingWindow' :: Int -> [Int] -> [[Int]]
slidingWindow' n [] = []
slidingWindow' n xs = (take n xs) : slidingWindow' n (tail xs)

process :: [Int] -> Int
process xs = length $ filter (\(a, b) -> a < b) $ zip xs (tail xs)

main :: IO ()
main = do
  input <- getContents
  let numbers = map read (lines input) :: [Int]
  print $ process $ map sum $ slidingWindow 3 numbers
