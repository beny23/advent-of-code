import Data.List
import Data.Char
import Data.Tuple (swap)
import qualified Data.MultiMap as M

type NodeMap = M.MultiMap String String

split :: Char -> String -> [String]
split d "" = []
split d xs = x : split d (drop 1 y) where (x,y) = span (/= d) xs

parseline :: String -> (String, String)
parseline s = (\[a, b] -> (a, b)) $ split '-' s

makeMap :: [(String, String)] -> NodeMap
makeMap xs = M.fromList $ filter onlyendto $ filter onlystartfrom $ xs ++ map swap xs

onlystartfrom (f, _) = f /= "end"
onlyendto (_, t) = t /= "start"

findPaths :: NodeMap -> [String] -> String -> Int
findPaths _ _ "end" = 1
findPaths nm prev from 
  | isSmallCave from && 
    visitedCave prev from > 1 - countDuplicateSmallCaves prev = 0
  | otherwise = sum $ map (findPaths nm (from:prev)) $ M.lookup from nm

isSmallCave :: String -> Bool
isSmallCave (x:xs) = isLower x

visitedCave :: [String] -> String -> Int
visitedCave prev cave = length $ filter (cave==) prev

countDuplicateSmallCaves :: [String] -> Int
countDuplicateSmallCaves prev = length $ filter (>1) $ map length $ group $ sort $ filter isSmallCave prev

main :: IO ()
main = do
  input <- getContents
  let raw = map parseline $ lines input
  let nm =  makeMap raw
  print $ findPaths nm [] "start"
