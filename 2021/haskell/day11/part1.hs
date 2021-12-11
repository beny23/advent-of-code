import Data.Map.Strict (Map, fromList, keys, updateLookupWithKey, mapAccum)

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

countFlashAndReset :: FlashMap -> (Int, FlashMap)
countFlashAndReset fm = mapAccum countFlashAndReset' 0 fm

countFlashAndReset' :: Int -> Int -> (Int, Int)
countFlashAndReset' acc f 
  | f > 9     = (acc + 1, 0)
  | otherwise = (acc, f)

iter :: (Int, FlashMap) -> (Int, FlashMap)
iter (acc, fm) = addCount acc $ countFlashAndReset $ incAll fm

addCount :: Int -> (Int, FlashMap) -> (Int, FlashMap)
addCount acc (acc', fm) = (acc + acc', fm)

iterN :: Int -> (Int, FlashMap) -> (Int, FlashMap)
iterN 0 fm = fm
iterN n fm = iterN (n-1) $ iter fm

main :: IO ()
main = do
  input <- getContents
  let initialMap = tomap $ map parseline $ lines input
  print $ fst $ iterN 100 (0, initialMap)
