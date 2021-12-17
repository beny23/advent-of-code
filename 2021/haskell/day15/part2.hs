import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Map.Strict ((!))
import Data.Maybe
import Control.Monad

type Point = (Int, Int)
type RiskMap = M.Map Point Int

type PQueue a = [(a, Int)]
type RiskQueue = PQueue Point

type DistMap = M.Map Point Int
type VisitedSet = S.Set Point

pqPush :: (a, Int) -> PQueue a -> PQueue a
pqPush e [] = [e]
pqPush e@(_, pri) qs@(q@(_, pri') : qs') 
  | pri < pri' = e : qs
  | otherwise  = q : pqPush e qs'

parseline :: String -> [Int]
parseline = map read . map pure

tomap :: [[Int]] -> RiskMap
tomap xs = M.fromList $ concat 
                      $ map maprows 
                      $ zip xs [1..]

maprows (row, y) = map (mapcols y) $ zip row [1..]
mapcols y (risk, x) = ((x, y), risk)

neighbours :: VisitedSet -> Point -> Point -> [Point]
neighbours visited d (x, y) = filter (\p -> S.notMember p visited) $ filter (onBoard d) [(x-1, y), (x, y-1), (x+1, y), (x, y+1)]

onBoard :: Point -> Point -> Bool
onBoard (w, h) (x, y)
  | x < 1     = False
  | y < 1     = False
  | x > w     = False
  | y > h     = False
  | otherwise = True

step :: Point -> RiskMap -> DistMap -> VisitedSet -> RiskQueue -> DistMap
step dim risks dists visited [] = dists
step dim risks dists visited ((p, d):qs) = 
  let ns = neighbours visited dim p
      ds = distances risks dists d ns
      dists' = M.union (M.fromList ds) dists
      qs' = foldl (\qs q -> pqPush q qs) qs ds
      visited' = S.insert p visited
  in step dim risks dists' visited' qs'

distances :: RiskMap -> DistMap -> Int -> [Point] -> [(Point, Int)]
distances risks dists d ns = ns >>= (distance risks dists d)

distance :: RiskMap -> DistMap -> Int -> Point -> [(Point, Int)]
distance risks dists d p = distance' p (dists ! p) (d + (risks ! p))

distance' :: Point -> Int -> Int -> [(Point, Int)]
distance' p d d' 
  | d' < d    = [(p, d')]
  | otherwise = []

transform :: [(Point, Int)] -> Point -> Point -> [(Point, Int)]
transform [] _ _ = []
transform (((x,y),r):ps) p@(x',y') d@(w, h) = ((x+x'*w,y+y'*h), ((r+x'+y'-1) `mod` 9) + 1) : transform ps p d

makeTransforms = [(x,y) | x <- [0..4], y <- [0..4]]

makeGrid :: [(Point, Int)] -> [(Point, Int)]
makeGrid points = 
  let dim = maximum $ map fst $ points
      ts = makeTransforms
  in concat $ map (\t -> transform points t dim) ts

main :: IO ()
main = do
  input <- getContents
  let risks = tomap $ map parseline $ lines input
  let points = M.keys risks
  let risks' = M.fromList $ makeGrid $ M.toList risks
  let end = maximum $ M.keys risks'
  let dists = M.adjust (const 0) (1, 1) $ M.map (const (maxBound :: Int)) risks'
  let q = M.toList dists
  let dists' = step end risks' dists S.empty q
  print $ dists' ! end
