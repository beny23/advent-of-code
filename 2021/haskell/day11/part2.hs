import Data.Map.Strict (Map, fromList, keys, updateLookupWithKey, mapAccum)
import qualified Data.Map.Strict as M (map, elems)

type Point = (Int, Int)
type FlashMap = Map Point Int

parseline :: String -> [Int]
parseline = map read . map pure

tomap :: [[Int]] -> FlashMap
tomap xs = fromList $ concat 
                    $ map maprows 
                    $ zip xs [1..]

maprows (row, y) = map (mapcols y) $ zip row [1..]
mapcols y (height, x) = ((x, y), height)

neighbours :: Point -> [Point]
neighbours (x, y) = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

incAll :: FlashMap -> FlashMap
incAll fm = foldl incOne fm $ keys fm

incOne :: FlashMap -> Point -> FlashMap
incOne fm p = triggerNeighbours p $ updateLookupWithKey (\p f -> Just (f+1)) p fm

triggerNeighbours :: Point -> (Maybe Int, FlashMap) -> FlashMap
triggerNeighbours p (Just f, fm)
  | f == 10   = foldl incOne fm $ neighbours p
  | otherwise = fm
triggerNeighbours _ (_, fm) = fm

resetAfterFlash :: FlashMap -> FlashMap
resetAfterFlash fm = M.map resetAfterFlash' fm

resetAfterFlash' :: Int -> Int
resetAfterFlash' f 
  | f > 9     = 0
  | otherwise = f

iter :: FlashMap -> FlashMap
iter fm = resetAfterFlash $ incAll fm

allZero :: FlashMap -> Bool
allZero fm = all (==0) $ M.elems fm

iterN :: FlashMap -> Int
iterN fm 
  | allZero fm = 0
  | otherwise  = 1 + (iterN $ iter fm)

main :: IO ()
main = do
  input <- getContents
  let initialMap = tomap $ map parseline $ lines input
  print $ iterN initialMap
