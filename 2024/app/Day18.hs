{-# LANGUAGE TupleSections #-}

module Day18 where

import Control.Monad
import Data.List (inits)
import Day16 (MinDist(Dist, Infinity), dijkstra)
import qualified Data.Set as S
import qualified Data.Map as M

type Pos = (Int, Int)
type Walls = S.Set Pos

parsePair :: String -> (Int, Int)
parsePair s = case break (==',') s of
    (a, ',':b) -> (read a, read b)
    _ -> error ""

nextStep :: (Int, Int) -> Walls -> MinDist -> Pos -> [(MinDist, Pos)]
nextStep dims walls _ pos = map (Dist 1,) (neighs pos)
    where

    inRange :: Int -> (Int, Int) -> Bool
    inRange x (a, b) = a <= x && x <= b

    inBounds :: (Int, Int) -> (Int, Int) ->  Bool
    inBounds (r, c) (h, w) = r `inRange` (0, h) && c `inRange` (0, w)

    neighs (r, c) = let ns = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
                     in filter (\n -> n `inBounds` dims && n `S.notMember` walls) ns

main :: IO ()
main = do
    walls <- map parsePair . lines <$!> readFile "data/Day18.txt"
    let dims = (70, 70)
        minDist walls = let dij = dijkstra (0, 0) (Dist 0) $ nextStep dims (S.fromList walls)
                         in M.findWithDefault Infinity dims dij
    print $ minDist (take 1024 walls)
    print $ head [ last ws | ws <- inits walls , minDist ws == Infinity ]
