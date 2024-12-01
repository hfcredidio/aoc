module Day4 where

import Control.Monad

type Range = (Int, Int)
type Pair = (Range, Range)

parseRange :: String -> Range
parseRange s = case break (=='-') s of
    (a, '-':b) -> (read a, read b)
    x          -> error $ show x

parsePair :: String -> Pair
parsePair s = case break (==',') s of
    (a, ',':b) -> (parseRange a, parseRange b)
    x          -> error $ show x

inRange :: Int -> Range -> Bool
inRange x (a, b) = a <= x && x <= b

isContainedBy :: Range -> Range -> Bool
isContainedBy (x, y) r = x `inRange` r && y `inRange` r

redundant :: Pair -> Bool
redundant = uncurry redundant'
    where redundant' r1 r2 = r1 `isContainedBy` r2 || r2 `isContainedBy` r1

overlap :: Pair -> Bool
overlap = uncurry overlap'
    where overlap' (a, b) p = a `inRange` p || b `inRange` p || redundant ((a, b), p)

main :: IO ()
main = do
    lines <- lines <$!> readFile "data/Day4.txt"
    print $ length $ filter redundant $ parsePair <$> lines
    print $ length $ filter overlap $ parsePair <$> lines
