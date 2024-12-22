{-# LANGUAGE TupleSections #-}

module Day08 where

import Control.Monad
import qualified Data.Map as M
import Data.List (nub)

type Position = (Int, Int)

(.+.) :: Position -> Position -> Position
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

(.-.) :: Position -> Position -> Position
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)

(.*.) :: Position -> Int -> Position
(x, y) .*. c = (c * x, c * y)

gdc :: Int -> Int -> Int
gdc x y | x == 0 || y == 0 = x + y
        | x == y = x
        | x > y = gdc (x - y) y
        | x < y = gdc x (y - x)
        | otherwise = error "A"

type Antenas = M.Map Char [Position]

readAntenas :: Int -> Int -> [String] -> Antenas
readAntenas nrows ncols ss = foldl insertAntena M.empty antennas
    where insertAntena ant (r, c, freq) = M.insertWith (++) freq [(r, c)] ant
          antennas = [(r, c, freq)
                     | (r, line) <- zip [0..nrows-1] ss
                     , (c, freq) <- zip [0..ncols-1] line
                     , freq /= '.'
                     ]

data Field = Field { fNrows :: Int
                   , fNcols :: Int
                   , fAntennas :: Antenas
                   } deriving(Show)

mkField :: [String] -> Field
mkField ss = Field nrows ncols antennas
    where nrows = length ss
          ncols = length (head ss)
          antennas = readAntenas nrows ncols ss

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs [_] = []
allPairs (x:xs) = map (x,) xs ++ map (,x) xs ++ allPairs xs

inBounds :: Int -> Int -> Position -> Bool
inBounds nrows ncols (r, c) = 0 <= r && r < nrows && 0 <= c && c < ncols

antiNodes :: Position -> Position -> Position
antiNodes p1 p2 = let delta = p2 .-. p1
                   in p1 .-. delta

allAntinodes :: Field -> [Position]
allAntinodes (Field nrows ncols antennas) = nub ans
    where ans= [ an
               | freq <- M.keys antennas
               , (a1, a2) <- allPairs $ antennas M.! freq
               , let an = antiNodes a1 a2
               , inBounds nrows ncols an ]

antiNodes' :: Position -> Position -> [Position]
antiNodes' p1 p2 = let (x, y) = p2 .-. p1
                       t = gdc (abs x) (abs y)
                       delta = (x`div`t, y`div`t)
                       deltas = map (delta.*.) [0..]
                    in map (p1.-.) deltas

allAntinodes' :: Field -> [Position]
allAntinodes' (Field nrows ncols antennas) = nub ans
    where ans= [ an'
               | freq <- M.keys antennas
               , (a1, a2) <- allPairs $ antennas M.! freq
               , let an = antiNodes' a1 a2
               , an' <- takeWhile (inBounds nrows ncols) an
               ]

main :: IO ()
main = do
    lines <- lines <$!> readFile "data/Day08.txt"
    print $ length . allAntinodes $ mkField lines
    print $ length . allAntinodes' $ mkField lines
