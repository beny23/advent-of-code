module Common where

import Text.Parsec
import Data.List
import Data.Maybe

data Elem = Lit Int | Pair Elem Elem deriving (Eq, Show)

type ElemList = [(String, Int)]

p_lit :: Parsec String () Elem
p_lit = do
  num <- read <$> many1 digit
  return (Lit num)

p_elem :: Parsec String () Elem
p_elem = p_pair <|> p_lit

p_pair :: Parsec String () Elem
p_pair = do
  char '['
  e1 <- p_elem
  char ','
  e2 <- p_elem
  char ']'
  return (Pair e1 e2)

parseLine :: String -> ElemList
parseLine s = tree2list "" $ right $ parse p_pair "" s

parseList :: ElemList -> Elem
parseList = list2tree ""

right :: Either a b -> b
right (Right x) = x

tree2list :: String -> Elem -> ElemList
tree2list path (Lit x) = [(path, x)]
tree2list path (Pair l r) = (tree2list (path ++ "0") l) ++ (tree2list (path ++ "1") r)

list2tree :: String -> ElemList -> Elem
list2tree p es = maybe (Pair (list2tree (p ++ "0") es) (list2tree (p ++ "1") es)) Lit (lookup p es)

split :: ElemList -> ElemList
split [] = []
split (e@(p,x):es)
  | x >= 10   = (p ++ "0", x `div` 2) : (p ++ "1", (x+1) `div` 2) : es
  | otherwise = e : split es

isExplodingPair :: String -> String -> Bool
isExplodingPair p1 p2
  | length p1 /= length p2 = False
  | length p1 < 5          = False
  | otherwise              = (stem p1) == (stem p2)

stem :: String -> String
stem p = take ((length p) - 1) p

explode :: ElemList -> ElemList
explode es = explode' [] es

explode' :: ElemList -> ElemList -> ElemList
explode' es' [e'] = reverse $ e':es'
explode' es' (e'@(p1, x1) : e@(p2, x2) : es)
  | isExplodingPair p1 p2 = (reverse $ addToFirst x1 $ es') ++ [(stem p1, 0)] ++ (addToFirst x2 es)
  | otherwise             = explode' (e':es') (e:es)

addToFirst :: Int -> ElemList -> ElemList
addToFirst _ [] = []
addToFirst x' ((p, x):es) = (p, x+x'):es

repeatUntil :: Eq a => (a -> a) -> a -> a
repeatUntil f e = repeatUntil' f e $ f e

repeatUntil' :: Eq a => (a -> a) -> a -> a -> a
repeatUntil' f e1 e2
  | e1 == e2  = e1
  | otherwise = repeatUntil f e2

reduce :: ElemList -> ElemList
reduce el = repeatUntil (split . repeatUntil explode) el

add :: ElemList -> ElemList -> ElemList
add e1 e2 = (map (prefixWith '0') e1) ++ (map (prefixWith '1') e2)

prefixWith :: Char -> (String, Int) -> (String, Int)
prefixWith c (p, x) = (c:p, x)

reduceAll :: [ElemList] -> ElemList
reduceAll [e] = e
reduceAll (e1 : e2 : es) = reduceAll ((reduce $ add e1 e2) : es)

magnitude :: Elem -> Int
magnitude (Lit x) = x
magnitude (Pair l r) = (3 * magnitude l) + (2 * magnitude r)
