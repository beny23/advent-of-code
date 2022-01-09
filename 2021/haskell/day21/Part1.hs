module Part1 where

import Common

play :: GameStatus -> Int -> DiceRolls -> Int
play g@((_, score1), (_, score2)) num _
  | score2 >= 1000 = score1 * num
play ((pos, score), p2) num (r1:r2:r3:dice) =
  let pos' = nextpos pos (r1+r2+r3)
      score' = score + pos'
  in play (p2, (pos', score')) (num+3) dice

main :: IO ()
main = do
  print $ play ((10, 0), (9, 0)) 0 diceRolls
