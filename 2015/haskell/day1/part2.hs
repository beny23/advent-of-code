bracketsToNum :: Char -> Int
bracketsToNum '(' = 1
bracketsToNum ')' = -1

findBasement :: String -> Int -> Int -> Int
findBasement _ (-1) p = p
findBasement (x:xs) f p = findBasement xs (f + bracketsToNum x) (p + 1)

main :: IO ()
main = do
  input <- getContents
  print $ findBasement input 0 0
