module Common where

type PlayerStatus = (Int, Int)
type GameStatus = (PlayerStatus, PlayerStatus)
type DiceRolls = [Int]

diceRolls :: DiceRolls
diceRolls = cycle [1..100]

nextpos :: Int -> Int -> Int
nextpos p n = (p + n - 1) `mod` 10 + 1
