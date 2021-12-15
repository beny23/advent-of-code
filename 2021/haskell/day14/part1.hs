import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List

type RuleMap = M.Map String Char

parseInsertionRule :: String -> (String, Char)
parseInsertionRule (a : b : ' ' : '-' : '>' : ' ' : c : []) = ([a, b], c)

parseInsertionRules :: [String] -> RuleMap
parseInsertionRules = M.fromList . map parseInsertionRule

nextGen :: RuleMap -> String -> String
nextGen rm [b] = [b]
nextGen rm (a : b : ss) = a : (rm ! [a, b]) : (nextGen rm (b : ss))

main :: IO ()
main = do
  input <- getContents
  let raw = lines input
  let template = head raw
  let ruleMap = parseInsertionRules $ tail $ tail raw
  let polymer = iterate (nextGen ruleMap) template !! 10
  let freq = sort $ map length $ group $ sort polymer
  print $ (head $ reverse freq) - (head freq)
