module Common where

import Text.Parsec
import Data.List
import Data.Maybe

type Pos = [Int]
type ScanList = [Pos]

p_num :: Parsec String () Int
p_num = do
  sign <- option ' ' (char '-')
  num <- many1 digit
  return (read $ sign : num)

p_nums :: Parsec String () Pos
p_nums = do
  nums <- p_num `sepBy1` (char ',')
  optional newline
  return nums

p_title :: Parsec String () Int
p_title = between (string "--- scanner ") (string " ---") p_num

p_scanner :: Parsec String () ScanList
p_scanner = do
  p_title
  newline
  nums <- many p_nums
  optional newline
  return nums

p_scanners :: Parsec String () [ScanList]
p_scanners = many p_scanner

normaliseFor :: Int -> ScanList -> ScanList
normaliseFor n s = map (zipWith (flip (-)) $ s !! n) s

rotateX :: Pos -> Pos
rotateX [x, y, z] = [x, -z, y]

rotateY :: Pos -> Pos
rotateY [x, y, z] = [-z, y, x]

rotateZ :: Pos -> Pos
rotateZ [x, y, z] = [y, x, -z]

rotate :: Pos -> [Pos]
rotate p = (take 4 $ iterate rotateX p) >>= (take 3 . iterate rotateY) >>= (take 2 . iterate rotateZ)

rotateAll :: ScanList -> [ScanList]
rotateAll = transpose . map rotate

countOverlaps :: ScanList -> ScanList -> Int
countOverlaps s s' = length $ s `intersect` s'

findMatch :: ScanList -> ScanList -> Maybe ScanList
findMatch s s' =
  let ns = sort s
      ns' = sort s'
      ls = (length s) - 11
      ls' = (length s') - 11
  in listToMaybe [nub $ sort $ ns ++ rs' | rs' <- map sort $ rotateAll s',
                                           n <- [0..ls],
                                           n' <- [0..ls'],
                                           countOverlaps (normaliseFor n ns) (normaliseFor n' rs') >= 12]

findAndMergeAll :: [ScanList] -> ScanList
findAndMergeAll (s:ss) = findAndMergeAll' s ss

findAndMergeAll' :: ScanList -> [ScanList] -> ScanList
findAndMergeAll' s [] = s
findAndMergeAll' s (s':ss') = findAndMergeMatch s s' ss' $ findMatch s s'

findAndMergeMatch :: ScanList -> ScanList -> [ScanList] -> Maybe ScanList -> ScanList
findAndMergeMatch s s' ss' Nothing = findAndMergeAll' s (ss' ++ [s'])
findAndMergeMatch s _ ss' (Just s') = findAndMergeAll' (nub $ sort $ s ++ s') ss'

