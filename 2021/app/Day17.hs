{-# LANGUAGE TupleSections #-}

module Day17 where

import Control.Monad
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)

type Point = (Int, Int)
data Box = Box Point Point deriving(Show)
data State = State { position :: Point
                   , velocity :: Point
                   , maxHeight :: Int
                   } deriving(Show)


stepState :: State -> State
stepState (State (x, y) (vx, vy) maxY) = (State (x', y') (vx', vy') maxY')
    where x' = x + vx
          y' = y + vy
          vx' = vx - signum vx
          vy' = vy - 1
          maxY' = max maxY y'


inBox :: Point -> Box -> Bool
inBox (x, y) (Box (a, b) (c, d)) = a <= x && x <= b && c <= y && y <= d


beyondBox :: Point -> Box -> Bool
beyondBox (x, y) (Box (_, b) (c, _)) = x > b || y < c


lastStateInBox :: State -> Box -> Maybe State
lastStateInBox init box = if lastPosition `inBox` box
                        then Just lastState
                        else Nothing
    where history  = iterate stepState init
          history' = takeWhile (not . (`beyondBox` box) . position) history
          lastState@(State lastPosition _ _) = last history'


velBounds :: Box -> (Point, Point)
velBounds (Box (a, b) (c, d)) = ((minVx, maxVx), (minVy, maxVy))
    where minVx = round $ sqrt $ fromIntegral $ 2 * a
          maxVx = b
          minVy = c
          maxVy = -c


searchMaxHeights :: Box -> [(Point, State)]
searchMaxHeights box = mapMaybe lastState testVels
    where lastState initVel = (initVel,) <$> lastStateInBox (State (0, 0) initVel 0) box
          ((minVx, maxVx), (minVy, maxVy)) = velBounds box
          testVels = (,) <$> [minVx..maxVx] <*> [minVy..maxVy]



getNumbers :: String -> [Int]
getNumbers [] = []
getNumbers ('-':c1:cs) | isDigit c1 = let (n:ns) = getNumbers (c1:cs)
                                       in -n:ns
getNumbers cs | isDigit $ head cs = let (ns, rest) = span isDigit cs
                                     in read ns:getNumbers rest
getNumbers (c:cs) = getNumbers cs


parseBox :: String -> Maybe Box
parseBox s = case getNumbers s of
    [a, b, c, d] -> Just $ Box (a, b) (c, d)
    _            -> Nothing



main :: IO ()
main = do
    box <- parseBox <$!> readFile "data/day17.txt"
    print $ maximum . map (maxHeight . snd) . searchMaxHeights <$> box
    print $ length . searchMaxHeights <$> box
