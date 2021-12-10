import Data.Maybe
import Data.List

parseline :: String -> String
parseline xs = parseline' xs []

parseline' :: String -> String -> String
parseline' ('(':cs) ss = parseline' cs ('(':ss)
parseline' ('<':cs) ss = parseline' cs ('<':ss)
parseline' ('[':cs) ss = parseline' cs ('[':ss)
parseline' ('{':cs) ss = parseline' cs ('{':ss)
parseline' (')':cs) ('(':ss) = parseline' cs ss
parseline' ('>':cs) ('<':ss) = parseline' cs ss
parseline' (']':cs) ('[':ss) = parseline' cs ss
parseline' ('}':cs) ('{':ss) = parseline' cs ss
parseline' (c:cs) _ = ""
parseline' _ ss  = ss

score :: Char -> Int
score '(' = 1
score '[' = 2
score '{' = 3
score '<' = 4

scoreline :: String -> Int
scoreline s = foldl (\acc c -> acc * 5 + score c) 0 s

mean :: [a] -> a
mean xs = head $ drop (length xs `div` 2) xs

main :: IO ()
main = do
  input <- getContents
  let incomplete = map parseline $ lines input
  print $ mean $ sort $ filter (/=0) $ map scoreline incomplete
