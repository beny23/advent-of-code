module Part1 where

import Common

main :: IO ()
main = do
  input <- getContents
  let numbers = map parseLine $ lines input
  let total = reduceAll numbers
  print $ magnitude $ parseList total