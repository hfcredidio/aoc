module Day11 where

import Control.Monad
import qualified Data.Set as S
import Data.List

type Site = (Int, Int)

inRange :: Int -> (Int, Int) -> Bool
inRange x (a, b) | a > b     = inRange x (b, a)
                 | otherwise = a <= x && x < b

manhattanDistance :: Int -> [Int] -> [Int] -> Site -> Site -> Int
manhattanDistance expansion er ec (r, c) (r', c') = dr + dc + (nr + nc) * (expansion - 1)
    where nr = length $ filter (`inRange` (r, r')) er
          nc = length $ filter (`inRange` (c, c')) ec
          dr = abs $ r - r'
          dc = abs $ c - c'

pairs :: [a] -> [b] -> [(a, b)]
pairs [] ys = []
pairs xs [] = []
pairs (x:xs) ys = [(x, y) | y <- ys] ++ pairs xs (tail ys)

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day11.txt"
    let nrows = length input
    let ncols = length (head input)
    let empty' row = all (=='.') row
    let emptyRows = [i | (i, row) <- zip [0..] input, empty' row]
    let emptyCols = [i | (i, row) <- zip [0..] (transpose input), empty' row]
    let stars = [(r, c)
                | (r, row) <- zip [0..] input
                , (c, col) <- zip [0..] row
                , col == '#'
                ]

    let distance1 = uncurry (manhattanDistance 2 emptyRows emptyCols)
    let distance2 = uncurry (manhattanDistance 1000000 emptyRows emptyCols)
    print $ sum $ distance1 <$> pairs stars stars
    print $ sum $ distance2 <$> pairs stars stars
