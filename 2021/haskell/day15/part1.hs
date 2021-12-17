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

pqPush :: (a, Int) -> PQueue a -> PQueue a
pqPush e [] = [e]
pqPush e@(_, pri) qs@(q@(_, pri') : qs') 
  | pri < pri' = e : qs
  | otherwise  = q : pqPush e qs'

pqRemove :: Eq a => a -> PQueue a -> PQueue a
pqRemove e [] = []
pqRemove e (q@(e', _) : qs)
  | e == e'   = qs
  | otherwise = q : pqRemove e qs

pqUpdate :: Eq a => PQueue a -> (a, Int) -> PQueue a
pqUpdate qs q@(e, pri) = pqPush q $ pqRemove e qs

parseline :: String -> [Int]
parseline = map read . map pure

tomap :: [[Int]] -> RiskMap
tomap xs = M.fromList $ concat 
                      $ map maprows 
                      $ zip xs [1..]

maprows (row, y) = map (mapcols y) $ zip row [1..]
mapcols y (risk, x) = ((x, y), risk)

neighbours :: RiskMap -> Point -> [Point]
neighbours risks (x, y) = filter (\p -> M.member p risks) [(x+1, y), (x, y+1)]

step :: RiskMap -> DistMap -> RiskQueue -> DistMap
step risks dists [] = dists
step risks dists ((p, d):qs) = 
  let ns = neighbours risks p
      ds = distances risks dists d ns
      dists' = M.union (M.fromList ds) dists
      qs' = foldl pqUpdate qs ds
      risks' = M.delete p risks
  in step risks' dists' qs'

distances :: RiskMap -> DistMap -> Int -> [Point] -> [(Point, Int)]
distances risks dists d ns = map (\p -> (p, min (dists ! p) (d + (risks ! p)))) ns

main :: IO ()
main = do
  input <- getContents
  let risks = tomap $ map parseline $ lines input
  let end = maximum $ M.keys risks
  let dists = M.adjust (const 0) (1, 1) $ M.map (const (maxBound :: Int)) risks
  let q = M.toList dists
  let dists' = step risks dists q
  print $ dists' ! end
