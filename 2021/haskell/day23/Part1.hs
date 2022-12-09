module Part1 where

import Common
import Data.List

main :: IO ()
main = do
  let init = setup [Pod Bronze, Pod Amber]
                   [Pod Copper, Pod Desert]
                   [Pod Bronze, Pod Copper]
                   [Pod Desert, Pod Amber]
  print $ process [(0, init)] Nothing