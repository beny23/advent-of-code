import Data.List

type Point = (Int, Int)

split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y) where (x,y) = span (/= d) xs

parsePoints :: [String] -> [Point]
parsePoints = map (\[x, y] -> (x,y)) . map (map read) . filter (\xs -> length xs == 2) . map (split ',') 

parseFolds :: [String] -> [Point]
parseFolds [] = []
parseFolds (x:xs)  
  | isPrefixOf "fold along y" x = (0, parseNum x) : parseFolds xs
  | isPrefixOf "fold along x" x = (parseNum x, 0) : parseFolds xs
  | otherwise                   = parseFolds xs

parseNum :: String -> Int
parseNum = read . tail . dropWhile (/='=')

foldAll :: Point -> [Point] -> [Point]
foldAll f = nub . map (foldOne f)

foldOne :: Point -> Point -> Point
foldOne (0, f) (px, py) = (px, foldOne' f py)
foldOne (f, 0) (px, py) = (foldOne' f px, py)

foldOne' :: Int -> Int -> Int
foldOne' f p
  | f > p = p
  | f < p = f - (p - f)

main :: IO ()
main = do
  input <- getContents
  let raw = lines input
  let points = parsePoints raw
  let folds = parseFolds raw
  print $ length $ foldAll (head folds) points
