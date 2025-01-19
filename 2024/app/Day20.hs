module Day20 where

import Data.List (sort)
import Day01 (countValues)
import Debug.Trace
import Control.Monad
import Data.Traversable
import qualified Data.Set as S
import qualified Data.Map as M

type Position = (Int, Int)

manhDist :: Position -> Position -> Int
manhDist (x, y) (x', y') = abs (x-x') + abs (y-y')

manhCircle :: Position -> Int -> [Position]
manhCircle (r, c) d = concat [ [(r+i, c+j), (r+j, c-i), (r-i, c-j), (r-j, c+i)] | i <- [0..d-1], let j = d - i]
        

manhDisk :: Position -> Int -> S.Set Position
manhDisk pos d = S.fromList $ concatMap (manhCircle pos) [0..d]

nextSteps :: S.Set Position -> Position -> Int -> [Position]
nextSteps floors pos d = S.toList $ manhDisk pos d `S.intersection` floors

getPath :: S.Set Position -> Position -> [Position]
getPath floor pos =
    let floor' = S.delete pos floor
        next = head $ nextSteps floor pos 1
    in if S.null floor'
        then [pos]
        else pos:getPath floor' next

timeToFinish :: [Position] -> M.Map Position Int
timeToFinish path = M.fromList [ (pos, totalTime - i) | (i, pos) <- zip [0..] path ]
    where totalTime = length path - 1

shortcuts :: S.Set Position -> M.Map Position Int -> Int -> Position -> [Int]
shortcuts floors mTimeToFinish cheat pos =
    map (totalTime-) $ filter (<totalTime) times
    where totalTime = length mTimeToFinish - 1
          currentTtime = totalTime - mTimeToFinish M.! pos
          ns = nextSteps floors pos cheat 
          times = [ currentTtime + manhDist pos n + mTimeToFinish M.! n | n <- ns ]

countShortcuts :: S.Set Position -> Position -> Int -> Int -> Int
countShortcuts floors start cheat minSave = length . filter (>=minSave) $ allShortcuts
    where path = getPath floors start
          totalTime = length path - 1       
          mTimeToFinish = timeToFinish path
          allShortcuts = concatMap (shortcuts floors mTimeToFinish cheat) path

main :: IO ()
main = do
    lines <- lines <$!> readFile "data/Day20.txt"
    let enumLines = [ (r, c, site) | (r, line) <- zip [0..] lines , (c, site) <- zip [0..] line ]
        floors = S.fromList [ (r, c) | (r, c, site) <- enumLines , site `elem` ".SE" ]
        start = head        [ (r, c) | (r, c, site) <- enumLines , site == 'S' ]
        dest  = head        [ (r, c) | (r, c, site) <- enumLines , site == 'E' ]

    print $ countShortcuts floors start 2 100
    print $ countShortcuts floors start 20 100
