tonum :: Char -> Int
tonum '0' = 0
tonum '1' = 1

bintodec :: String -> Int
bintodec ns = foldl (\s n -> 2 * s + (tonum n)) 0 ns

split :: [String] -> ([String], [String])
split xs = split' xs ([], [])

split' :: [String] -> ([String], [String]) -> ([String], [String])
split' [] x = x
split' (s:ss) (as, bs)
  | (head s) == '1' = split' ss (tail s : as, bs)
  | otherwise       = split' ss (as, tail s : bs)

pick :: (Int -> Int -> Bool) -> ([String], [String]) -> String
pick f (as, bs)
  | f (length as) (length bs) = '0' : (pickall f bs)
  | otherwise                 = '1' : (pickall f as)

pickall :: (Int -> Int -> Bool) -> [String] -> String
pickall f [x] = x
pickall f xs = pick f $ split xs

main :: IO ()
main = do
  input <- getContents
  let readings = lines input
  let oxygen = pickall (<) readings
  let co2 = pickall (>=) readings
  print ((bintodec oxygen) * (bintodec co2))
