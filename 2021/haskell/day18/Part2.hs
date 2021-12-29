module Part2 where

import Common

main :: IO ()
main = do
  input <- getContents
  let numbers = map parseLine $ lines input
  let pairs = [(x1, x2) | x1 <- numbers, x2 <- numbers, x1 < x2]
  let totals = map (magnitude . parseList . reduce . (uncurry add)) pairs
  print $ maximum totals