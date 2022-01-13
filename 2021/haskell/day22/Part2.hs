module Part1 where

import Common

main :: IO ()
main = do
  input <- getContents
  let instructions = parseLines $ lines input
  print $ sum $ map cuboids $ foldl mergeAll [] instructions
