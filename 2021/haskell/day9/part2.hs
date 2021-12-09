import Data.Map (Map, fromList, keys, (!))
import Data.Map as M (lookup)
import Data.Maybe (maybeToList)
import Data.List (nub, sort)

type Point = (Int, Int)
type HeightMap = Map Point Int

parseline :: String -> [Int]
parseline = map read . map (\x -> [x])

tomap :: [[Int]] -> HeightMap
tomap xs = fromList $ concat 
                    $ map maprows 
                    $ zip xs [1..]

maprows (row, y) = map (mapcols y) $ zip row [1..]
mapcols y (height, x) = ((x, y), height)

findLowestPoints :: HeightMap -> [Point]
findLowestPoints hm = filter (isLowestPoint hm) $ keys hm

neighbours :: Point -> [Point]
neighbours (x, y) = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

isLowestPoint :: HeightMap -> Point -> Bool
isLowestPoint hm p = 
  let maybeNeighbours = map (\p -> M.lookup p hm) (neighbours p)
  in (hm ! p) < (minimum [p | Just p <- maybeNeighbours])

isHigher :: Int -> Maybe Int -> Bool
isHigher h (Just h') = h < h' && h' < 9
isHigher _ _ = False

findHigherPoints :: HeightMap -> Point -> [Point]
findHigherPoints hm p =
  let height = hm ! p
      higherNeighbours = filter (\p -> isHigher height (M.lookup p hm)) (neighbours p)
  in p : (concat $ map (findHigherPoints hm) higherNeighbours)

main :: IO ()
main = do
  input <- getContents
  let heights = map parseline $ lines input
  let heightMap = tomap heights
  let lowestPoints = findLowestPoints heightMap
  print $ product $ take 3 $ reverse $ sort $ map length $ map nub $ map (findHigherPoints heightMap) lowestPoints
