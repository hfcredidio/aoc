module Day17 where

import Control.Monad

import Data.Char (isDigit)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (minimumBy, delete)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Matrix as Mt
import qualified Data.PQueue.Prio.Min as Q

type Grid = Mt.Matrix Int

data Direction = U | D | L | R deriving(Show, Eq, Ord)

data Node = Node { nodeDirection :: Direction
                 , nodeCount     :: Int
                 , nodeCoord     :: (Int, Int)
                 } deriving(Show, Eq, Ord)

data Graph = Graph { gNeighbors :: Node -> [Node]
                   , gWeight    :: Node -> Node -> Int
                   }

opposite :: Direction -> Direction
opposite U = D
opposite D = U
opposite L = R
opposite R = L

moveCoord :: Direction -> (Int, Int) -> (Int, Int)
moveCoord U (r, c) = (r - 1, c)
moveCoord D (r, c) = (r + 1, c)
moveCoord L (r, c) = (r, c - 1)
moveCoord R (r, c) = (r, c + 1)

moveNode :: Int -> Int -> Direction -> Node -> Maybe Node
moveNode minCount maxCount d (Node d' c rc)
    | d == opposite d' = Nothing
    | d == d'&& c >= maxCount = Nothing
    | d == d'&& c <  maxCount = Just $ Node d (c + 1)  rc'
    | d /= d'&& c <  minCount = Nothing
    | otherwise               = Just $ Node d 1 rc'
    where rc' = moveCoord d rc

inBound :: Grid -> (Int, Int) -> Bool
inBound g (r, c) = inR && inC
    where inR = 1 <= r && r <= Mt.nrows g
          inC = 1 <= c && c <= Mt.ncols g

neighbors :: Int -> Int -> Grid -> Node -> [Node]
neighbors minCount maxCount g n = filter (inBound g . nodeCoord) ns
    where ns = catMaybes $ moveNode minCount maxCount <$> [R, D, U, L] <*> [n]

weight :: Grid -> Node -> Node -> Int 
weight grid _ (Node _ _ rc) = grid Mt.! rc

data DijState = DijState { dijPrevious :: M.Map Node Node
                         , dijDistance :: M.Map Node Int
                         , dijQueue    :: Q.MinPQueue Int Node
                         , dijVisited  :: S.Set Node
                         } deriving(Show)

inf = 10^10

setMany :: Ord k => [k] -> [v] -> M.Map k v -> M.Map k v
setMany ks vs m = M.fromList (zip ks vs) `M.union` m

setManyOn :: Ord k => [k] -> (k -> v) -> M.Map k v -> M.Map k v
setManyOn ks f m = setMany ks (map f ks) m

qInsertManyOn :: Ord k
              => (v -> k)
              -> [v]
              -> Q.MinPQueue k v
              -> Q.MinPQueue k v
qInsertManyOn f vs q = foldr (\v -> Q.insert (f v) v) q vs

dij :: Graph -> (Int, Int) -> DijState -> DijState
dij g dst st@(DijState prev dist Q.Empty _) = st

dij g dst st@(DijState prev dist ((_, current)Q.:<queue) visited)
    | current `S.member` visited =
        dij g dst st { dijQueue = queue }

dij g dst st@(DijState prev dist ((_, current)Q.:<queue) visited)
  | nodeCoord current == dst = st

dij g dst st@(DijState prev dist ((_, current)Q.:<queue) visited) =
    dij g dst (DijState prev' dist' queue' visited')
    where neighbors  = gNeighbors g current
          getDist  n = fromMaybe inf $ M.lookup n dist
          tentDist n = getDist current + gWeight g current n
          isBetter n = tentDist n < getDist n
          selected   = filter isBetter neighbors
          dist'      = setManyOn selected tentDist dist
          prev'      = setMany   selected (repeat current) prev
          queue'     = qInsertManyOn tentDist selected queue
          visited'   = S.insert current visited

shortestPath :: Graph -> (Int, Int) -> (Int, Int) -> (Int, [(Int, Int)])
shortestPath g src dst = (sPathDist, sPath)
    where start = Node U 100 src
          st = DijState { dijPrevious = M.empty
                        , dijDistance = M.singleton start 0
                        , dijQueue    = Q.singleton 0 start
                        , dijVisited  = S.empty }
          (DijState prev dist _ _) = dij g dst st
          end  = snd $ minimum [ (d, n)
                               | (n, d) <- M.toList dist
                               , (nodeCoord n) == dst
                               ]
          tracePath n | n `M.notMember` prev = [n]
          tracePath n = n:tracePath (prev M.! n)
          sPath = map nodeCoord $ reverse $ tracePath end
          sPathDist = dist M.! end

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day17.txt"
    let nrows = length input
    let ncols = length (head input)
    let numbers = map (read . pure) $ concat input :: [Int]
    let grid = Mt.fromList nrows ncols numbers

    let graph = Graph { gNeighbors = neighbors 1 3 grid
                      , gWeight = weight grid
                      }
    print $ fst $ shortestPath graph (1, 1) (nrows, ncols)

    let graph = Graph { gNeighbors = neighbors 4 10 grid
                      , gWeight = weight grid
                      }
    print $ fst $ shortestPath graph (1, 1) (nrows, ncols)
