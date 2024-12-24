module Day12 where

import Control.Monad
import Data.List (sort)
import qualified Data.Matrix as M
import qualified Data.Set as S

type Position = (Int, Int)
data Direction = N | E | W | S deriving(Eq, Ord, Show)
data Border = Border Direction Int Int deriving(Show, Eq, Ord)

mkBorder :: Direction -> Position -> Border
mkBorder N (r, c) = Border N r c
mkBorder S (r, c) = Border S r c
mkBorder E (r, c) = Border E c r
mkBorder W (r, c) = Border W c r

move :: Direction -> Position -> Position
move N (r, c) = (r-1, c)
move S (r, c) = (r+1, c)
move W (r, c) = (r, c-1)
move E (r, c) = (r, c+1)

getNeighPos :: Int -> Int -> Position -> [Position]
getNeighPos nrows ncols (r, c) = filter inBounds ns
    where ns = move <$> [N, S, W, E] <*> [(r, c)]
          inBounds (r, c) = 0 < r && r <= nrows && 0 < c && c <= ncols

getGroup' :: M.Matrix Char -> S.Set Position -> Position -> S.Set Position
getGroup' m acc pos@(r, c) = foldl (getGroup' m) acc' neigh
    where val = M.getElem r c m
          neigh = [ n 
                  | n@(nr, nc) <- getNeighPos (M.nrows m) (M.ncols m) pos
                  , M.getElem nr nc m == val 
                  , n `S.notMember` acc ]
          acc' = S.union acc (S.fromList neigh)

getGroup :: M.Matrix Char -> Position -> S.Set Position
getGroup m p = getGroup' m (S.singleton p) p

getAllGroups' :: M.Matrix Char -> [S.Set Position] -> Position -> [S.Set Position]
getAllGroups' m acc pos
    | any (pos`S.member`) acc = acc
    | otherwise = getGroup m pos:acc

getAllGroups :: M.Matrix Char -> [S.Set Position]
getAllGroups m = foldl (getAllGroups' m) [] pos
    where pos = (,) <$> [1..M.nrows m] <*> [1..M.ncols m]
    
getBorder' :: S.Set Position -> Position -> [Border]
getBorder' s pos = [ mkBorder dir pos
                   | dir <- [N,S,W,E]
                   , let n = move  dir pos
                   , n `S.notMember` s
                   ]

getBorder :: S.Set Position -> [Border]
getBorder s = sort $ concatMap (getBorder' s) (S.toList s)

countSides :: [Border] -> Int
countSides [] = 0
countSides [_] = 1
countSides (Border dir a b:x@(Border dir' a' b'):xs) =
    let rest = countSides (x:xs)
    in if dir == dir' && a == a' && b + 1 == b'
        then rest
        else 1 + rest

main :: IO ()
main = do
    sites <- lines <$!> readFile "data/Day12.txt"
    let mat = M.fromLists sites
    let grps = getAllGroups mat
    print $ sum [ S.size g * length (getBorder g) | g <- grps ]
    print $ sum [ S.size g * countSides (getBorder g) | g <- grps ]
