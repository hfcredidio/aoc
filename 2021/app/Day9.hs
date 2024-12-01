module Day9 where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S


data Site = Valley { depth :: Int }
          | Peak
          deriving(Show)

isPeak :: Site -> Bool
isPeak Peak = True
isPeak _    = False

parseSite :: Char -> Site
parseSite '9' = Peak
parseSite c   = Valley $ read $ pure c

type Position = (Int, Int)

class Graph g where
    graphGet :: g -> Position -> Maybe Site
    graphElem :: g -> Position -> Bool
    neighbors :: g -> Position -> S.Set Position


breadthFirst' :: Graph g => S.Set Position -> [Position] -> g -> [Position]
breadthFirst' visited [] g = []
breadthFirst' visited (p:qs) g = if p `S.member` visited then rest else p:rest
        where ns = neighbors g p
              newVisited = S.insert p visited
              newQueue = qs ++ S.toList (ns `S.difference` visited)
              rest = breadthFirst' newVisited newQueue g

breadthFirst g p = breadthFirst' S.empty [p] g

newtype Grid = Grid (M.Matrix Site) deriving(Show)

instance Graph Grid where
    graphGet (Grid g) p = uncurry M.safeGet p g
    graphElem (Grid g) (r, c) = 1 <= r &&  r <= M.nrows g && 1 <= c && c <= M.ncols g
    neighbors grid@(Grid g) p@(r, c) = case graphGet grid p of
        Nothing   -> S.empty
        Just Peak -> S.empty
        Just _    -> S.fromList noPeaks
            where ns = [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1)]
                  isPeak' = maybe False isPeak . graphGet grid
                  noPeaks = filter (not . isPeak') $ filter (graphElem grid) ns


peaks :: Grid -> S.Set Position
peaks (Grid g) = S.fromList [(r, c) | r <- [1..M.nrows g], c <- [1..M.ncols g], isPeak $ M.getElem r c g]

basinSizes' :: S.Set Position -> Grid -> [Int]
basinSizes' visited grid@(Grid g) = let gridPoints = [(r, c) | r <- [1..M.nrows g], c <- [1..M.ncols g]] in
    case filter (`S.notMember` visited) gridPoints of
      []    -> []
      (p:_) -> length basin:basinSizes' newVisited grid
          where basin = breadthFirst grid p
                newVisited = visited `S.union` S.fromList basin

basinSizes grid = basinSizes' (peaks grid) grid


isLower :: Site -> Site -> Bool
isLower Peak Peak = False
isLower Peak _    = True
isLower _    Peak = True
isLower (Valley d1) (Valley d2) = d1 < d2


isLowPoint :: Grid -> Position -> Bool
isLowPoint grid@(Grid g) p@(r, c) = all (site `isLower`) ns
    where site = M.getElem r c g
          ns = [M.getElem nr nc g | (nr, nc) <- S.toList $ neighbors grid (r, c)]


riskValue :: Grid -> Int
riskValue grid@(Grid g) = sum $ M.mapPos siteRisk g
    where siteRisk (r, c) Peak = 0
          siteRisk (r, c) (Valley v) = if isLowPoint grid (r, c) then v + 1 else 0


main :: IO ()
main = do
    grid <- Grid . M.fromLists . map (map parseSite) . lines <$!> readFile "data/day9.txt"
    print $ riskValue grid
    print $ product $ take 3 $ sortOn negate $ basinSizes grid
