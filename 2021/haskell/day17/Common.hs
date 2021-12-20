module Common where

import Data.List

type Point = (Int, Int)
type Rect = (Point, Point)

isHit :: Point -> Rect -> Bool
isHit (x, y) ((x1, y1), (x2, y2))
  | x < x1    = False
  | x > x2    = False
  | y < y1    = False
  | y > y2    = False
  | otherwise = True

canHit :: Rect -> Point -> Point -> Bool
canHit target@((_, y'), (x', _)) p@(x, y) (vx, vy)
  | x > x'    = False
  | y < y'    = False
  | otherwise = isHit p target || canHit target (x+vx, y+vy) (max (vx-1) 0, vy-1)

maxX :: Rect -> Int
maxX target = fst $ snd $ target

minY :: Rect -> Int
minY target = snd $ fst $ target

findHits :: Rect -> Point -> [Point]
findHits target start =
  let vs = [(x, y) | x <- [          1 .. maxX target],
                     y <- [minY target .. 20 * (maxX target)]]
  in filter (canHit target start) vs

findHighest :: [Point] -> Int
findHighest ps = (\y -> (y * (y + 1)) `div` 2) $ maximum $ map snd ps