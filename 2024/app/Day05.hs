module Day05 where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Char (isDigit)
import Data.List (elemIndex, sort)
import Control.Monad

parseEdge :: String -> (Int, Int)
parseEdge s = case span (/='|') s of
    (a, '|':b) | all isDigit a && all isDigit b -> (read a, read b)
    _ -> error $ "Invalid Edge " ++ show s

parseList :: String -> [Int]
parseList "" = []
parseList (',':s) = parseList s
parseList  s = read num:parseList rest
    where (num, rest) = span isDigit s


data Graph a = Graph { gNodes_     :: S.Set a
                     , gEdges_     :: M.Map a [a]
                     , gInDegrees_ :: M.Map a Int
                     } deriving (Show)

gNodes :: Ord a => Graph a -> [a]
gNodes = S.toList . gNodes_

gNeighbors :: Ord a => Graph a -> a -> [a]
gNeighbors g n = M.findWithDefault [] n $ gEdges_ g

gInDegree :: Ord a => Graph a -> a -> Int
gInDegree g n = M.findWithDefault 0 n $ gInDegrees_ g

addEdge :: Ord a => Graph a -> (a, a) -> Graph a
addEdge (Graph nodes edges indegs) (from, to) = Graph nodes' edges' indegs'
    where edges'  = M.insertWith (++) from [to] edges
          indegs' = M.insertWith (+) to 1 indegs
          nodes'  = foldr S.insert nodes [from, to]

mkGraph :: Ord a => [(a, a)] -> Graph a
mkGraph = foldl addEdge (Graph S.empty M.empty M.empty)

popHeads :: Ord a => Graph a -> ([a], Graph a)
popHeads g@(Graph nodes edges indeg) = (heads, g')
    where heads  = [n | n <- gNodes g, gInDegree g n == 0 ]
          nodes' = foldr S.delete nodes heads
          edges'  = foldr M.delete edges heads
          neighs = [n' | n <- heads, n' <- gNeighbors g n]
          indeg' = foldr (M.adjust (\x -> x-1)) indeg neighs
          g' = Graph nodes' edges' indeg'

gSubset :: Ord a => Graph a -> [a] -> Graph a
gSubset (Graph _ edges _) ns = mkGraph edges'
    where edges' = [(from, to)
                   | from <- ns
                   , to <- M.findWithDefault [] from edges
                   , to `elem` ns]

topSortOrder :: Ord a => Graph a -> [a]
topSortOrder g
    | S.null (gNodes_ g) = []
    | otherwise = let (heads, g') = popHeads g
                   in heads ++ topSortOrder g'

sortWith :: Ord b => Ord a => (a -> b) -> [a] -> [a]
sortWith key xs = map snd $ sort [(key x, x) | x <- xs]

topSort :: Ord a => Graph a -> [a] -> [a]
topSort g xs = let g' = gSubset g xs 
                in sortWith (`elemIndex` topSortOrder g') xs

isTopSorted :: Ord a => Graph a -> [a] -> Bool
isTopSorted g xs = let g' = gSubset g xs
                    in and $ zipWith (==) (topSort g xs) xs

midElem :: [a] -> a
midElem xs = xs !! (length xs `div` 2)

main :: IO ()
main = do
    (edgesRaw, _:orderingsRaw) <- span (/="") . lines <$!> readFile "data/Day05.txt"
    let orderings = map parseList orderingsRaw
    let graph  = mkGraph $ map parseEdge edgesRaw

    let ordered = filter (isTopSorted graph) orderings
    print $ sum $ map midElem ordered

    let unordered = filter (not . isTopSorted graph) orderings
    print $ sum $ map (midElem . topSort graph) unordered
