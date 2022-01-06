module Part1 where

import Text.Parsec
import Data.List
import Common

main :: IO ()
main = do
  input <- getContents
  let raw = lines input
  let algo = parseLine $ head raw
  let image = addBorder 150 $ map parseLine $ tail $ tail raw
  let images = iterate (processLines algo) image
  print $ length $ filter (==1) $ concat $ images !! 50