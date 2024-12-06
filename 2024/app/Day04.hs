module Day04 where

import Control.Monad
import qualified Data.Matrix as M

data Direction = DU | DD | DL | DR | DUL | DUR | DDL | DDR

moveDir :: Int -> Int -> Direction -> (Int, Int)
moveDir r c DU  = (r - 1, c)
moveDir r c DD  = (r + 1, c)
moveDir r c DL  = (r, c - 1)
moveDir r c DR  = (r, c + 1)
moveDir r c DUL = (r - 1, c - 1)
moveDir r c DUR = (r - 1, c + 1)
moveDir r c DDL = (r + 1, c - 1)
moveDir r c DDR = (r + 1, c + 1)

directions = [DU , DD , DL , DR , DUL , DUR , DDL , DDR]

listMatch :: Eq a => M.Matrix a -> Int -> Int -> Direction -> [a] -> Bool
listMatch _ _ _ _ [] = True
listMatch m r c dir (x:xs) = case M.safeGet r c m of
    Nothing         -> False
    Just y | y /= x -> False
    Just y          -> let (r', c') = moveDir r c dir
                        in listMatch m r' c' dir xs

listCrossMatch :: Eq a => M.Matrix a -> Int -> Int -> Direction -> [a] -> Int -> Bool
listCrossMatch m r c d xs n = case d of
    DDR -> listMatch m r c DDR xs && (listMatch m r (c+n) DDL xs || listMatch m (r+n) c DUR xs)
    DDL -> listMatch m r c DDL xs && (listMatch m r (c-n) DDR xs || listMatch m (r+n) c DUL xs)
    DUR -> listMatch m r c DUR xs && (listMatch m r (c+n) DUL xs || listMatch m (r-n) c DDR xs)
    DUL -> listMatch m r c DUL xs && (listMatch m r (c-n) DUR xs || listMatch m (r-n) c DDL xs)
    _ -> False


count :: [Bool] -> Int
count = length . filter id

main :: IO ()
main = do
    mat <- M.fromLists . lines <$!> readFile "data/Day04.txt"
    let f = listMatch mat <$> [0..M.nrows mat] <*> [0..M.ncols mat] <*> directions
    print $ count $ f <*> ["XMAS"]
    let g = listCrossMatch mat <$> [0..M.nrows mat] <*> [0..M.ncols mat] <*> [DDL, DDR, DUL, DUR]
    print $ (`div`2) . count $ g <*> ["MAS"] <*> [2]
