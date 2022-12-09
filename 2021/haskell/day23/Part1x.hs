module Part1x where

import Common
import Data.List

main :: IO ()
main = do
  let init = [Empty, Cave Amber [Pod Bronze, Pod Amber], Empty, Cave Bronze [Pod Amber, Pod Copper], Empty, Cave Copper [Pod Copper, Pod Bronze], Empty, Empty]
  print $ process [(0, init)] Nothing