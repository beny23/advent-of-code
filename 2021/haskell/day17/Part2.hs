module Part2 where

import Common

main :: IO ()
main = do
  -- 14..50, y=-267..-225
  let target = ((14, -267), (50, -225))
  let start  = (0, 0)
  print $ length $ findHits target start