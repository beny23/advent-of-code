module Common where

import Text.Parsec
import Text.Parsec.Char
import Data.List

type Vec = [Int]
type Cube = [Vec]
type Instruction = (Cube, Int)

p_num :: Parsec String () Int
p_num = do
  sign <- option ' ' (char '-')
  num <- many1 digit
  return (read $ sign : num)

p_range :: Parsec String () Vec
p_range = do
  start <- p_num
  string ".."
  end <- p_num
  return [start, end+1]

p_cube :: Parsec String () Cube
p_cube = do
  string "x="
  x <- p_range
  string ",y="
  y <- p_range
  string ",z="
  z <- p_range
  return $ sort $ transpose [x, y, z]

p_on :: Parsec String () Int
p_on = do
  string "on"
  return 1

p_off :: Parsec String () Int
p_off = do
  string "off"
  return (-1)

p_instruction :: Parsec String () Instruction
p_instruction = do
  status <- try p_on <|> p_off
  string " "
  cube <- p_cube
  return (cube, status)

parseLines :: [String] -> [Instruction]
parseLines = map (\(Right x) -> x) . map (parse p_instruction "")

isInner :: Cube -> Bool
isInner [a, b] = all (>=(-50)) a && all (<=51) b

volume :: Cube -> Int
volume [a, b] = product $ zipWith (\a' b' -> max 0 $ b' - a') a b

filterInner :: [Instruction] -> [Instruction]
filterInner = filter (isInner . fst)

inter :: Cube -> Cube -> Cube
inter [a1, a2] [b1, b2] = [zipWith max a1 b1, zipWith min a2 b2]

cuboids :: Instruction -> Int
cuboids (c, n) = n * volume c

merge :: Instruction -> Instruction -> Instruction
merge (a, 1) (b, 1) = (inter a b, -1)
merge (a, -1) (b, -1) = (inter a b, 1)
merge (a, 1) (b, -1) = (inter a b, -1)
merge (a, -1) (b, 1) = (inter a b, 1)

overlaps :: Instruction -> Instruction -> Bool
overlaps (c, _) (c', _) = (>0) $ volume $ inter c c'

findOverlaps :: [Instruction] -> Instruction -> [Instruction]
findOverlaps is i = filter (overlaps i) is

mergeAll :: [Instruction] -> Instruction -> [Instruction]
mergeAll is i = dedupe $ (mergeAll' is i) ++ (map (`merge` i) $ findOverlaps is i)

mergeAll' :: [Instruction] -> Instruction -> [Instruction]
mergeAll' is i@(_, 1) = i : is
mergeAll' is (_, -1) = is

dedupe :: [Instruction] -> [Instruction]
dedupe = filter ((/=0) . snd)
       . map (\xs -> (fst $ head xs, sum $ map snd xs))
       . groupBy (\a b -> (fst a) == (fst b))
       . sort

dump :: Cube -> [Vec]
dump [[x1,y1,z1], [x2,y2,z2]] =
  [[x,y,z] | x <- [x1..x2-1],
             y <- [y1..y2-1],
             z <- [z1..z2-1]]