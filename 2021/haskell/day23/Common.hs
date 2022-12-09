module Common where

import Data.List
import Data.Maybe

data Type = Amber | Bronze | Copper | Desert deriving (Eq, Show)
data Tile = Empty | Cave Type [Tile] | Pod Type deriving (Eq, Show)

type State = (Int, [Tile])

cost :: Type -> Int
cost Amber = 1
cost Bronze = 10
cost Copper = 100
cost Desert = 1000

setup :: [Tile] -> [Tile] -> [Tile] -> [Tile] -> [Tile]
setup as bs cs ds =
  [Empty, Empty,
   Cave Amber as,
   Empty,
   Cave Bronze bs,
   Empty,
   Cave Copper cs,
   Empty,
   Cave Desert ds,
   Empty, Empty]

reverseState :: State -> State
reverseState (c, ts) = (c, reverse ts)

move :: Int -> [Tile] -> [Tile] -> [State]
move c [] ts' = []
move c (t@(Pod tp):ts) ts' = (enter c tp ts (Empty:ts')) ++ map reverseState (enter c tp ts' (Empty:ts)) ++ (move c ts (t:ts'))
move c (t@(Cave tp [Pod tp1, Pod tp2]):ts) ts'
  | tp1 /= tp || tp2 /= tp = (exit c tp1 ts ((Cave tp [Empty, Pod tp2]):ts')) ++
                map reverseState (exit c tp1 ts' ((Cave tp [Empty, Pod tp2]):ts)) ++
                (move c ts (t:ts'))
move c (t@(Cave tp [Empty, Pod tp2]):ts) ts'
  | tp2 /= tp = (exit (c + cost tp2) tp2 ts ((Cave tp [Empty, Empty]):ts')) ++
                map reverseState (exit (c + cost tp2) tp2 ts' ((Cave tp [Empty, Empty]):ts)) ++
                (move c ts (t:ts'))
move c (t:ts) ts' = move c ts (t:ts')

enter :: Int -> Type -> [Tile] -> [Tile] -> [State]
enter _ _ [] _ = []
enter c tp (t@(Cave tp' [Empty, Empty]):ts) ts'
  | tp == tp' = [(c + 3 * cost tp, (reverse ts') ++ [Cave tp [Empty, Pod tp]] ++ ts)] ++ enter (c + cost tp) tp ts (t:ts')
enter c tp (t@(Cave tp' [Empty, Pod tp'']):ts) ts'
  | tp == tp' && tp == tp'' = [(c + 2 * cost tp, (reverse ts') ++ [Cave tp [Pod tp, Pod tp]] ++ ts)] ++ enter (c + cost tp) tp ts (t:ts')
enter c tp ((Pod _):ts) ts' = []
enter c tp (t:ts) ts' = enter (c + cost tp) tp ts (t:ts')

exit :: Int -> Type -> [Tile] -> [Tile] -> [State]
exit _ _ [] _ = []
exit c tp (Empty:ts) ts' = [(c + 2 * cost tp, (reverse ts') ++ [Pod tp] ++ ts)] ++ exit (c + cost tp) tp ts (Empty:ts')
exit c tp ((Pod _):ts) ts' = []
exit c tp (t:ts) ts' = exit (c + cost tp) tp ts (t:ts')

complete :: [Tile] -> Bool
complete [] = True
complete (Empty:ts) = complete ts
complete ((Pod _):ts) = False
complete ((Cave tp cs):ts) = all (complete' tp) cs && complete ts

complete' :: Type -> Tile -> Bool
complete' tp (Pod tp') = tp == tp'
complete' _ _ = False

sortMoves :: [State] -> [State]
sortMoves = sortOn fst

mergeMoves :: [State] -> [State] -> [State]
mergeMoves [] ss2 = ss2
mergeMoves ss1 [] = ss1
mergeMoves ss1@(s1@(c1,_):ss1') ss2@(s2@(c2,_):ss2')
  | c1 < c2   = s1 : mergeMoves ss1' ss2
  | c2 > c2   = s2 : mergeMoves ss1 ss2'
  | otherwise = s1 : s2 : mergeMoves ss1' ss2'

process :: [State] -> Maybe State -> State
process [] (Just lowest) = lowest
process ((newScore, _):ss) (Just lowest@(lowestScore, _))
  | newScore > lowestScore = lowest
process ((c,ts):ss) maybeLowest =
  let moved = move c ts []
      newLowest = listToMaybe $ sortMoves $ (filter (complete . snd) moved) ++ (maybeToList maybeLowest)
      newStates = mergeMoves ss $ (filter (not . complete . snd) moved)
  in process newStates newLowest