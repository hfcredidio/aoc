module Day12 where

import Control.Monad
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

data Node = Start | End | Small String | Large String deriving(Eq, Show)


isSmall :: Node -> Bool
isSmall (Small _) = True
isSmall _         = False


instance Ord Node where
    compare a b | a == b = EQ
    compare Start _      = LT
    compare End   _      = GT
    compare _     Start  = GT
    compare _     End    = LT
    compare (Small s) (Small z) = compare s z
    compare (Large s) (Large z) = compare s z
    compare (Small s) (Large z) = LT
    compare (Large s) (Small z) = GT


parseNode :: String -> Node
parseNode "start" = Start
parseNode "end" = End
parseNode s | all isLower s = Small s
            | all isUpper s = Large s
            | otherwise     = error $ "Invalid node " ++ show s


parseEdge :: String -> (Node, Node)
parseEdge s = (parseNode a, parseNode b)
    where (a, _:b) = span (/='-') s


mkGraph :: [(Node, Node)] -> M.Map Node [Node]
mkGraph [] = M.empty
mkGraph ((a, b):nodes) = g'
    where g  = mkGraph nodes
          g' = M.insertWith (++) a [b] $ M.insertWith (++) b [a] g


allPathsToEnd' :: Node -> S.Set Node -> M.Map Node [Node] -> [[Node]]
allPathsToEnd' End  visited graph = [[End]]
allPathsToEnd' node visited graph = map (node:) rest
        where newVisited = if isSmall node then S.insert node visited else visited
              ns = filter (`S.notMember` newVisited) $ M.findWithDefault [] node graph
              rest = concatMap (\n -> allPathsToEnd' n newVisited graph) ns

allPathsToEnd = allPathsToEnd' Start (S.singleton Start)


allPathsToEnd2' :: Node -> S.Set Node -> M.Map Node [Node] -> [[Node]]
allPathsToEnd2' End  visited graph = [[End]]
allPathsToEnd2' node visited graph = map (node:) rest
        where newVisited = if isSmall node then S.insert node visited else visited
              ns = filter (/=Start) $ M.findWithDefault [] node graph
              rest = if isSmall node && node `S.member` visited
                then let ns' = filter (`S.notMember` newVisited) ns in
                     concatMap (\n -> allPathsToEnd'  n newVisited graph) ns'

                else concatMap (\n -> allPathsToEnd2' n newVisited graph) ns

allPathsToEnd2 = allPathsToEnd2' Start (S.singleton Start)


main :: IO ()
main = do
    edges <- mkGraph . map parseEdge . lines <$!> readFile "data/day12.txt"
    print $ length $ allPathsToEnd edges
    print $ length $ allPathsToEnd2 edges
