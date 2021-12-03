process :: [Int] -> Int
process xs = length $ filter (\(a, b) -> a < b) $ zip xs (tail xs)

main :: IO ()
main = do
  input <- getContents
  let numbers = map read (lines input) :: [Int]
  print $ process numbers
