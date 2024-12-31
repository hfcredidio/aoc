{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances  #-}

module Day16 where

import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.PQueue.Prio.Min as Q
import Control.Monad
import Data.List (nub)

-- based on
-- https://acatalepsie.fr/posts/haskell-dijkstra.html
-- https://webcf.waybackmachine.org/web/20241228070542/https://acatalepsie.fr/posts/haskell-dijkstra.html

class (Eq a, Ord a, Monoid a) => Weight a where
  infty :: a
  updateWeight :: a -> a -> a
  updateWeight = const

data MinDist = Dist !Int | Infinity deriving (Eq, Ord, Show)
instance Semigroup MinDist where
    Infinity <> _ = Infinity
    _ <> Infinity = Infinity
    Dist x <> Dist y = Dist (x + y)
instance Monoid MinDist where mempty = Dist 0
instance Weight MinDist where infty  = Infinity

data ShortestPaths a = ShortestPaths MinDist [[a]] deriving(Show)
instance Eq (ShortestPaths a) where
    ShortestPaths x _ == ShortestPaths y _ = x == y
instance Ord (ShortestPaths a) where
    ShortestPaths x _ `compare` ShortestPaths y _ = x `compare` y
instance Semigroup (ShortestPaths a) where
    ShortestPaths x xss <> ShortestPaths y yss = ShortestPaths (x <> y) zss
        where zss = (<>) <$> xss <*> yss
instance Monoid (ShortestPaths a) where
    mempty = ShortestPaths mempty mempty
instance Weight (ShortestPaths a) where
    infty = ShortestPaths Infinity []
    updateWeight (ShortestPaths x xss) (ShortestPaths _ yss) = ShortestPaths x (xss <> yss)

dijkstra :: forall w n . (Weight w, Show w, Ord n)
         => n
         -> w
         -> (w -> n -> [(w, n)])
         -> M.Map n w
dijkstra src initCost next =
    let it = iterate aux (M.singleton src initCost, Q.singleton initCost src)
    in fst . last . takeWhile (not . Q.null . snd) $ it

    where

    aux :: (M.Map n w, Q.MinPQueue w n) -> (M.Map n w, Q.MinPQueue w n)
    aux (costs, Q.Empty) = (costs, Q.empty)
    aux (costs, (uWeight, u)Q.:<queue) =
        let uWeight' = M.findWithDefault infty u costs
        in if uWeight' == uWeight
            then let edges = next uWeight' u
                  in foldl (relaxNeighbor uWeight') (costs, queue) edges
            else (costs, queue)

    relaxNeighbor :: w
                  -> (M.Map n w, Q.MinPQueue w n)
                  -> (w, n)
                  -> (M.Map n w, Q.MinPQueue w n)
    relaxNeighbor uWeight (costs, queue) (uvWeight, v) =
        let vWeight  = uWeight <> uvWeight
            vWeight' = M.findWithDefault infty v costs
        in case compare vWeight vWeight' of
            GT -> (costs, queue)
            EQ -> (M.insert v (updateWeight vWeight' vWeight) costs, queue)
            LT -> (M.insert v vWeight costs, Q.insert vWeight v queue)

---
data Direction = N | S | W | E deriving(Eq, Show, Ord)
data Orientation = CW | CCW deriving(Eq, Show, Ord)
type Position = (Int, Int)
data Reindeer = Reindeer { rPos :: Position, rDir :: Direction } deriving(Eq, Ord, Show)
data Move = StepForward | Turn Orientation deriving(Eq, Show, Ord)
type Walls = S.Set Position

rMove :: Reindeer -> Move -> (ShortestPaths Position, Reindeer)
rMove (Reindeer pos dir) move =
    let (dist, reind') = case move of
            StepForward -> (Dist 1, Reindeer (stepForward pos dir) dir)
            Turn or     -> (Dist 1000, Reindeer pos (turn dir or))
    in (ShortestPaths dist [[rPos reind']], reind')

    where 

    turn :: Direction -> Orientation -> Direction
    turn N CW = E
    turn E CW = S
    turn S CW = W
    turn W CW = N
    turn N CCW = W
    turn W CCW = S
    turn S CCW = E
    turn E CCW = N

    stepForward :: Position -> Direction -> Position
    stepForward (r, c) N = (r - 1, c)
    stepForward (r, c) S = (r + 1, c)
    stepForward (r, c) E = (r, c + 1)
    stepForward (r, c) W = (r, c - 1)


rNext :: Walls -> ShortestPaths Position -> Reindeer -> [(ShortestPaths Position, Reindeer)]
rNext w _ r = filter ((`S.notMember`w) . rPos . snd) xs
    where xs = rMove r <$> [StepForward, Turn CCW, Turn CW]

main :: IO ()
main = do
    lines <- lines <$!> readFile "data/Day16.txt"
    let enumLines = [ (r, c, site) | (r, line) <- zip [1..] lines , (c, site) <- zip [1..] line ]
    let walls = S.fromList [ (r, c)            | (r, c, site) <- enumLines , site == '#' ]
    let reind = head       [ Reindeer (r, c) E | (r, c, site) <- enumLines , site == 'S' ]
    let dest  = head       [ (r, c)            | (r, c, site) <- enumLines , site == 'E' ]
    let initCost = ShortestPaths mempty [[rPos reind]]
    let res = dijkstra reind initCost (rNext walls)
    let ShortestPaths minDist paths = minimum [ res M.! Reindeer dest d | d <- [N, S, E, W] ]
    print minDist
    print $ length . nub $ concat paths
