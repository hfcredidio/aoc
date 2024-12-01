{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances  #-}

module Day15 where

import Control.Monad
import qualified Data.Matrix as M
import qualified Data.Set as S


parseLine :: String -> [Int]
parseLine = map (read . pure)

parseFile :: String -> M.Matrix Int
parseFile = M.fromLists . map parseLine . lines


class Graph g a where
    graphElem :: g -> a -> Bool
    graphNeigh :: g -> a -> [a]
    graphWeight :: Num w => g -> a -> a -> w


type Site = (Int, Int)
newtype SimpleMatrix = SimpleMatrix (M.Matrix Int)
newtype ExtendedMatrix = ExtendedMatrix (M.Matrix Int)


instance Graph SimpleMatrix Site where
    graphElem (SimpleMatrix m) (i, j) = 1 <= i && i <= M.nrows m && 1 <= j && j <= M.ncols m
    graphNeigh m (i, j) = filter (graphElem m) ns
        where ns = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
    graphWeight (SimpleMatrix m) _ (i, j) = fromIntegral $ M.getElem i j m


instance Graph ExtendedMatrix Site where
    graphElem (ExtendedMatrix m) (i, j) = 1 <= i && i <= 5 * M.nrows m && 1 <= j && j <= 5 * M.ncols m
    graphNeigh m (i, j) = filter (graphElem m) ns
        where ns = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
    graphWeight (ExtendedMatrix m) _ (i, j) = fromIntegral $ ((total - 1) `mod` 9) + 1
        where (ri, ni) = divMod i $ M.nrows m
              (rj, nj) = divMod j $ M.ncols m
              total = M.getElem (ni + 1) (nj + 1) m + ri + rj


graphStep :: (Num cost, Ord node, Graph graph node) => graph -> (cost, node) -> [(cost, node)]
graphStep graph (cost, node) = zip costs ns
    where ns = graphNeigh graph node
          costs = map ((cost+) . graphWeight graph node) ns


shortestPath' :: (Num cost, Ord cost, Ord node, Graph graph node)
              => S.Set node
              -> S.Set (cost, node)
              -> graph
              -> node
              -> Maybe (cost, node)
shortestPath' visited queue graph target = case S.minView queue of
     Nothing -> Nothing
     Just ((cost, node), popped)
        | node == target          -> Just (cost, node)
        | node `S.member` visited -> shortestPath' visited popped graph target 
        | otherwise               -> shortestPath' visited' queue' graph target
        where visited' = S.insert node visited
              queue' = foldr S.insert popped $ graphStep graph (cost, node)

shortestPath graph target start = shortestPath' S.empty (S.singleton start) graph target


main :: IO ()
main = do
    grid <- parseFile <$!> readFile "data/day15.txt"

    let start = (1, 1)
    let end = (M.nrows grid, M.ncols grid)
    print $ fst <$> shortestPath (SimpleMatrix grid) end (0, start)

    let end = (5 * M.nrows grid, 5 * M.ncols grid)
    print $ fst <$> shortestPath (ExtendedMatrix grid) end (0, start)
