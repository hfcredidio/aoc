module Day11 where

import Control.Monad
import Data.List
import qualified Data.Matrix as M
import qualified Data.Dequeue as D

type Position = (Int, Int)
type Grid = M.Matrix Int

inGrid :: Grid -> Position -> Bool
inGrid g (r, c) = 1 <= r && r <= M.nrows g && 1 <= c && c <= M.ncols g

neighbors :: Grid -> Position -> [Position]
neighbors g (r, c) = filter (inGrid g) ns
    where ns = [(r + 1, c + 1)
               ,(r + 1, c    )
               ,(r + 1, c - 1)
               ,(r    , c + 1)
               ,(r    , c - 1)
               ,(r - 1, c + 1)
               ,(r - 1, c    )
               ,(r - 1, c - 1)]


increment :: Grid -> Position -> Grid
increment g (r, c) | M.getElem r c g == 0 = g
                   | otherwise            = M.setElem (M.getElem r c g + 1) (r, c) g


flash :: D.BankersDequeue Position -> Grid -> (Grid, Int)
flash queue g = case D.popFront queue of
    Nothing               -> (g, 0)
    Just ((r, c), popped) -> (g''', count + 1)
        where ns = neighbors g (r, c)
              g' = foldl increment g ns
              g'' = M.setElem 0 (r, c) g'
              newQueue = foldl D.pushBack popped [(r, c) | (r, c) <- ns, M.getElem r c g'' == 10]
              (g''', count) = flash newQueue g''


step :: Grid -> (Grid, Int)
step g = flash queue g'
    where g' = M.mapPos (\_ x -> x+1) g
          queue = D.fromList [(r, c) | r <- [1..M.nrows g], c <- [1..M.ncols g], M.getElem r c g' == 10]



parseGrid :: [String] -> Grid
parseGrid = M.fromLists . map (map (read . pure))


main :: IO ()
main = do
    grid <- parseGrid . lines <$!> readFile "data/day11.txt"
    print $ sum $ take 101 $ map snd $ iterate (step . fst) (grid, 0)
    let gridSize = M.ncols grid * M.nrows grid
    print $ elemIndex gridSize $ map snd $ iterate (step . fst) (grid, 0)
