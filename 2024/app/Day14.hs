module Day14 where

import Day01 (countValues)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Control.Monad

type Vec2 = (Int, Int)

map2 :: (a -> b) -> (a, a) -> (b, b)
map2 f (x, y) = (f x, f y)

ap2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
ap2 (f, g) (x, y) = (f x, g y)

op2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
op2 f v1 v2 = map2 f v1 `ap2` v2

(.+) = op2 (+)
(.-) = op2 (-)
(.*) c =  map2 (c*)
mod2 = op2 mod

data Robot = Robot { rX :: Vec2, rV :: Vec2 } deriving(Show, Eq)
stepRobot :: Vec2 -> Int -> Robot -> Robot
stepRobot dims nsteps (Robot x v) = Robot (x' `mod2` dims) v
    where x' = (nsteps .* v) .+ x

parseVec2 :: String -> Vec2
parseVec2 (_:'=':xs) = map2 read (lhs, rhs)
    where (lhs, _:rhs) = break (==',') xs

parseRobot :: String -> Robot
parseRobot s = case words s of
    [pos, vel] -> Robot (parseVec2 pos) (parseVec2 vel)
    _ -> error "A"

quadrant :: Vec2 -> Vec2 -> Maybe Int
quadrant (dx, dy) (x, y) = do
    qx <- quad dx x
    qy <- quad dy y
    return (qy * 2 + qx)
    where quad dim x = let h = dim`div`2
                in case compare x h of
                    EQ -> Nothing
                    LT -> Just 0
                    GT -> Just 1

quadrantCount :: Vec2 -> [Robot] -> [Int]
quadrantCount dims rs = M.elems . countValues $ quads
    where quads = catMaybes [ quadrant dims pos | Robot pos _ <- rs]

safetyFactor :: Vec2 -> [Robot] -> Int
safetyFactor dims = product . quadrantCount dims

showRob :: Vec2 -> [Robot] -> String
showRob (dx, dy) rs =
    unlines [ [ if (x,y) `elem` xs then '#' else '.'
      | x <- [0..dx-1]
      ]
    | y <- [0..dy-1] ]
    where xs = rX <$> rs
          
main :: IO ()
main = do
    robots <- map parseRobot . lines <$!> readFile "data/Day14.txt"

    let dims = (101, 103)
        robots' = stepRobot dims 100 <$> robots
    print $ safetyFactor dims robots'

    let robotsHist = iterate (map $ stepRobot dims 1) robots
        safetyHist = map (safetyFactor dims) robotsHist
        (_, iter) = minimum $ take 10000 $ zip safetyHist [0..]
    putStrLn $ showRob dims (robotsHist !! iter)
    print iter
