import Data.Set

move :: Char -> (Int, Int) -> (Int, Int)
move '^' (x, y) = (x, y+1)
move 'v' (x, y) = (x, y-1)
move '<' (x, y) = (x-1, y)
move '>' (x, y) = (x+1, y)

travel :: [Char] -> Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
travel [] s p = insert p s
travel (c:cs) s p = travel cs (insert p s) (move c p)

main :: IO ()
main = do
  input <- getContents
  let coords = travel input empty (0,0)
  print $ size coords
