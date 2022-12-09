module Part1 where

import Text.Parsec
import Data.List
import Common

main :: IO ()
main = do
  input <- getContents
  let numbers = (\(Right s) -> s) $ parse p_scanners "" input
  let merged = findAndMergeAll numbers
  print numbers
  print merged
  print $ length merged