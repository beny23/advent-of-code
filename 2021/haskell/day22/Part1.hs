module Part1 where

import Common
import qualified Data.Set as S

type CubeSet = S.Set Vec

mergeSet :: CubeSet -> Instruction -> CubeSet
mergeSet cs (c, 1) = cs `S.union` (S.fromList $ dump c)
mergeSet cs (c, -1) = cs `S.difference` (S.fromList $ dump c)

main :: IO ()
main = do
  input <- getContents
  let instructions = filterInner $ parseLines $ lines input
  print $ S.size $ foldl mergeSet S.empty instructions
