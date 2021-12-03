add :: [Int] -> [Char] -> [Int]
add [] [] = []
add (n:ns) ('1':cs) = (n+1) : add ns cs
add (n:ns) ('0':cs) = n : add ns cs

base :: [String] -> [Int]
base (x:xs) = take (length x) $ repeat 0

tobin :: (Int -> Bool) -> [Int] -> [Int]
tobin f ns = map (tobin' . f) ns

tobin' :: Bool -> Int
tobin' True = 1
tobin' False = 0

bintodec :: [Int] -> Int
bintodec ns = foldl (\s n -> 2 * s + n) 0 ns

main :: IO ()
main = do
  input <- getContents
  let readings = lines input
  let totals = foldl add (base readings) readings
  let half_count = (length readings) `div` 2
  let gamma = tobin (>half_count) totals
  let epsilon = map (1-) gamma
  print ((bintodec gamma) * (bintodec epsilon))
