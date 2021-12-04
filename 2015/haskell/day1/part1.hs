bracketsToNum :: Char -> Int
bracketsToNum '(' = 1
bracketsToNum ')' = -1

main :: IO ()
main = do
  input <- getContents
  let floor = sum $ map bracketsToNum input
  print floor
