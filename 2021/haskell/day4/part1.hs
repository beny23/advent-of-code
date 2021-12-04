import Data.Maybe
import Data.List

split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y) where (x,y) = span (/= d) xs

calledNumbers :: String -> [Int]
calledNumbers s = map read $ split ',' s

readvec :: [(Int, Int)] -> String -> [(Int, Int)]
readvec cwp s = map (\x -> (x, fromJust $ lookup x cwp)) $ map read $ words s

readmatrices :: [(Int, Int)] -> [String] -> [[[(Int, Int)]]]
readmatrices cwp [] = []
readmatrices cwp (b:r1:r2:r3:r4:r5:xs) = (map (readvec cwp) [r1, r2, r3, r4, r5]) : readmatrices cwp xs

comparepos :: (Int, Int) -> (Int, Int) -> Ordering
comparepos (_, a) (_, b) 
  | a < b     = LT
  | a > b     = GT
  | otherwise = EQ

comparematrix :: ([[(Int, Int)]], (Int, Int)) -> ([[(Int, Int)]], (Int, Int)) -> Ordering
comparematrix (_, a) (_, b) = comparepos a b

maxpos :: [[(Int, Int)]] -> (Int, Int)
maxpos vecs = minimumBy comparepos $ map (maximumBy comparepos) (vecs ++ transpose vecs)

calculatescore :: ([[(Int, Int)]], (Int, Int)) -> Int
calculatescore (mat, (lastnum, lastpos)) = lastnum * (sum $ map fst $ filter (\(num, pos) -> pos > lastpos) $ concat mat)

main :: IO ()
main = do
  input <- getContents
  let text = lines input
  let called = calledNumbers (head text)
  let calledWithPos = zip called [1..]
  let matrices = readmatrices calledWithPos (tail text)
  let bingo = minimumBy comparematrix $ map (\m -> (m, maxpos m)) matrices
  print $ calculatescore bingo
