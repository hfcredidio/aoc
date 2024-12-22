module Day10 where

import Control.Monad
import qualified Data.Matrix as M
import Data.List (nub)


readDig :: Char -> Int
readDig '.' = 99
readDig c = read (pure c)

type Position = (Int, Int)
type Trail = [Position]

neighs :: Position -> [Position]
neighs (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

getTrails :: Position -> M.Matrix Int -> [Trail]
getTrails pos@(r, c) m = case M.safeGet r c m of
    Nothing -> []
    Just 9 -> [[pos]]
    Just x | x < 9 -> [ pos:path
                      | n@(nr, nc) <- neighs pos
                      , M.safeGet nr nc m == Just (x+1)
                      , path <- getTrails n m ]
    _ -> error "A"

trailsScore :: [Trail] -> Int
trailsScore [] = 0
trailsScore paths = length . nub $ map last paths

main :: IO ()
main = do
    digs <- map (map readDig) . lines <$!> readFile "data/Day10.txt" :: IO [[Int]]
    let mat = M.fromLists digs
        trails = [ getTrails (r, c) mat
                 | r <- [1..M.nrows mat]
                 , c <- [1..M.ncols mat]
                 , M.getElem r c mat == 0
                 ]
    print $ sum $ map trailsScore trails
    print $ sum $ map length trails
