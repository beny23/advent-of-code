parseline :: String -> Int
parseline s = length $ filter (\len -> any (==len) [2,4,3,7]) $ map length $ dropWhile (/="|") $ words s

main :: IO ()
main = do
  input <- getContents
  print $ sum $ map parseline $ lines $ input
