module Part2 where

import Common
import Data.List

diceFreq :: [(Int, Int)]
diceFreq = map (\x -> (head x, length x))
             $ group
             $ sort
             $ [a+b+c | a <- [1..3],
                        b <- [1..3],
                        c <- [1..3]]

nextPlayer :: Int -> Int
nextPlayer 1 = 2
nextPlayer 2 = 1

countWinner :: (GameStatus, Int) -> [Int]
countWinner (((_, score1), (_, score2)), num)
  | score1 > score2 = [num, 0]
  | otherwise       = [0, num]

checkGameover :: (GameStatus, Int) -> Bool
checkGameover (((_, score1), (_, score2)), _) = max score1 score2 >= 21

dedupe :: [(GameStatus, Int)] -> [(GameStatus, Int)]
dedupe gs = map (\gs -> (fst $ head gs, sum $ map snd gs)) $ groupBy (\a b -> (fst a) == (fst b)) $ sort gs

play :: Int -> [(GameStatus, Int)] -> [Int]
play player games =
  let games' = games >>= turn player
      deduped = dedupe games'
      finished = all checkGameover deduped
  in play' player deduped finished

play' :: Int -> [(GameStatus, Int)] -> Bool -> [Int]
play' _ gs True = foldl (zipWith (+)) [0,0] $ map countWinner gs
play' player gs False = play (nextPlayer player) gs

turn :: Int -> (GameStatus, Int) -> [(GameStatus, Int)]
turn player x@(gs, num)
  | checkGameover x = [x]
  | otherwise       = map (\(roll, freq) -> (playerTurn player gs roll, num * freq)) diceFreq

playerTurn :: Int -> GameStatus -> Int -> GameStatus
playerTurn 1 (p1, p2) roll = (turn' p1 roll, p2)
playerTurn 2 (p1, p2) roll = (p1, turn' p2 roll)

turn' :: PlayerStatus -> Int -> PlayerStatus
turn' (pos, score) roll =
  let pos' = nextpos pos roll
      score' = score + pos'
  in (pos', score')

main :: IO ()
main = do
  print $ maximum $ play 1 [(((10, 0), (9, 0)), 1)]
