module Part1 where

import Text.Parsec
import Data.List
import Common

main :: IO ()
main = do
  input <- getContents
  let raw = lines input
  let algo = parseLine $ head raw
  let image = map parseLine $ tail $ tail raw
  let image1 = processLines algo $ addBorder 8 image
  let image2 = processLines algo image1
  print $ length $ filter (==1) $ concat $ image2