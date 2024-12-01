module Day14 where

import Control.Monad
import qualified Data.Map as M


type Pair = (Char, Char)
type ReplaceRules = M.Map Pair Char
type PairCounts = M.Map Pair Integer


replacePair :: ReplaceRules -> Pair -> [Pair]
replacePair m p@(a, b) = case M.lookup p m of
    Nothing -> [(a, b)]
    Just i  -> [(a, i), (i, b)]


updatePairCount' :: ReplaceRules  -> [(Pair, Integer)] -> PairCounts
updatePairCount' _ [] = M.empty
updatePairCount' replRules ((pair, count):ps) = counts'
    where newPairs = replacePair replRules pair
          increment m k = M.insertWith (+) k count m
          counts = updatePairCount' replRules ps
          counts' = foldl increment counts newPairs

updatePairCount :: ReplaceRules -> PairCounts -> PairCounts
updatePairCount replRule = updatePairCount' replRule . M.toList


countValues :: Ord a => [a] -> M.Map a Integer
countValues []     = M.empty
countValues (x:xs) = M.insertWith (+) x 1 counts
    where counts = countValues xs


charCounts' :: M.Map Char Integer -> Char -> Char -> [(Pair, Integer)] -> M.Map Char Integer
charCounts' partial firstChar lastChar [] = halved
    where tmp  = M.insertWith (+) firstChar 1 partial
          tmp' = M.insertWith (+) lastChar 1 tmp
          halved = M.map (`div` 2) tmp'
charCounts' partial firstChar lastChar ((pair, count):ps) = res
    where (a, b) = pair
          counts'  = M.insertWith (+) a count partial
          counts'' = M.insertWith (+) b count counts'
          res = charCounts' counts'' firstChar lastChar ps

charCounts firstChar lastChar = charCounts' M.empty firstChar lastChar . M.toList


score :: Char -> Char -> PairCounts -> Integer
score firstChar lastChar pairCounts = maximum values - minimum values
    where values = map snd $ M.toList $ charCounts firstChar lastChar pairCounts


parseFile :: String -> (PairCounts, ReplaceRules, Char, Char)
parseFile s = (countValues pairs, parseReplaceRules b, head a, last a)
    where (a:_, _:b) = span (/="") $ lines s
          pairs = zip a $ tail a


parseReplaceRules :: [String] -> ReplaceRules
parseReplaceRules []     = M.empty
parseReplaceRules (l:ls) = M.insert pair repl rest
    where a:b:_ = takeWhile (/=' ') l
          pair = (a, b)
          repl = last l
          rest = parseReplaceRules ls


main :: IO ()
main = do
    (pairCounts, replRules, firstChar, lastChar) <- parseFile <$!> readFile "data/day14.txt"
    print $ score firstChar lastChar $ (!!10) $ iterate (updatePairCount replRules) pairCounts
    print $ score firstChar lastChar $ (!!40) $ iterate (updatePairCount replRules) pairCounts
