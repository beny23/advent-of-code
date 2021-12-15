import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List

type RuleMap = M.Map String [String]
type PairMap = M.Map String Int

parseInsertionRule :: String -> (String, [String])
parseInsertionRule (a : b : ' ' : '-' : '>' : ' ' : c : []) = ([a, b], [[a, c], [c, b]])

parseInsertionRules :: [String] -> RuleMap
parseInsertionRules = M.fromList . map parseInsertionRule

mkInitialPairMap :: String -> PairMap
mkInitialPairMap s = M.fromList $ map (\(a, b) -> ([a, b], 1)) $ zip s $ tail s

nextGen :: RuleMap -> PairMap -> PairMap
nextGen rules pairs = foldl (M.unionWith (+)) M.empty $ map (uncurry M.singleton) $ nextGen' rules $ M.toList pairs

nextGen' :: RuleMap -> [(String, Int)] -> [(String, Int)]
nextGen' rules pairs = do
  (pair, count) <- pairs
  nextPair <- rules ! pair
  [(nextPair, count)]

main :: IO ()
main = do
  input <- getContents
  let raw = lines input
  let template = head raw
  let pairs = mkInitialPairMap template
  let rules = parseInsertionRules $ tail $ tail raw
  let polymer = iterate (nextGen rules) pairs !! 40
  let lastLetter = M.singleton (head $ reverse template) 1
  let freq = sort $ map snd 
                  $ M.toList
                  $ foldl (M.unionWith (+)) lastLetter 
                  $ map (\(a, b) -> M.singleton (head a) b)
                  $ M.toList polymer
  print $ (head $ reverse freq) - (head freq)
