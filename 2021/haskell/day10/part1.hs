import Data.Maybe

parseline :: String -> Maybe Char
parseline xs = parseline' xs []

parseline' :: String -> String -> Maybe Char
parseline' ('(':cs) ss = parseline' cs ('(':ss)
parseline' ('<':cs) ss = parseline' cs ('<':ss)
parseline' ('[':cs) ss = parseline' cs ('[':ss)
parseline' ('{':cs) ss = parseline' cs ('{':ss)
parseline' (')':cs) ('(':ss) = parseline' cs ss
parseline' ('>':cs) ('<':ss) = parseline' cs ss
parseline' (']':cs) ('[':ss) = parseline' cs ss
parseline' ('}':cs) ('{':ss) = parseline' cs ss
parseline' (c:cs) _ = Just c
parseline' _ _  = Nothing

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

main :: IO ()
main = do
  input <- getContents
  let corruptChars = map parseline $ lines input
  print $ sum $ map score [c | Just c <- corruptChars]