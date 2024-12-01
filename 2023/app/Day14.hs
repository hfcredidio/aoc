module Day14 where

import Control.Monad
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

type Ball = (Int, Int)
type Pegs = M.Map Int (S.Set Int)
data Dish = Dish { nRows :: Int
                 , nCols :: Int
                 , pegsPerCol :: Pegs
                 , pegsPerRow :: Pegs
                 } deriving(Show)

data Direction = North | South | West | East deriving(Show) 

rollBall :: Dish -> Direction -> Ball -> (Dish, Ball)

rollBall dish North (r, c) = (dish', (r'', c))
    where rows = pegsPerCol dish M.! c
          r' = fromMaybe 0 $ S.lookupLE r rows
          r'' = head $ filter (`S.notMember` rows) $ map (r'+) [1..]
          ppc' = M.update (Just . S.insert r'') c (pegsPerCol dish)
          dish' = dish { pegsPerCol = ppc' }

rollBall dish South (r, c) = (dish', (r'', c))
    where rows = pegsPerCol dish M.! c
          r' = fromMaybe (nRows dish + 1) $ S.lookupGE r rows 
          r'' = head $ filter (`S.notMember` rows) $ map (r'-) [1..]
          ppc' = M.update (Just . S.insert r'') c (pegsPerCol dish)
          dish' = dish { pegsPerCol = ppc' }

rollBall dish West (r, c) = (dish', (r, c''))
    where cols = pegsPerRow dish M.! r
          c' = fromMaybe 0 $ S.lookupLE c cols
          c'' = head $ filter (`S.notMember` cols) $ map (c'+) [1..]
          ppr' = M.update (Just . S.insert c'') r (pegsPerRow dish)
          dish' = dish { pegsPerRow = ppr' }

rollBall dish East (r, c) = (dish', (r, c''))
    where cols = pegsPerRow dish M.! r
          c' = fromMaybe (nCols dish + 1) $ S.lookupGE c cols 
          c'' = head $ filter (`S.notMember` cols) $ map (c'-) [1..]
          ppr' = M.update (Just . S.insert c'') r (pegsPerRow dish)
          dish' = dish { pegsPerRow = ppr' }

rollBalls :: Dish -> Direction -> [Ball] -> [Ball]
rollBalls dish dir [] = []
rollBalls dish dir (ball:bs) = ball':rollBalls dish' dir bs
    where (dish', ball') = rollBall dish dir ball

rollCycle :: Dish -> [Ball] -> [Ball]
rollCycle d bs = sort $ roll East $ roll South $ roll West $ roll North bs
    where roll = rollBalls d

dishLoad :: Dish -> [Ball] -> Int
dishLoad d bs = sum $ map ((nc-) . fst) bs
    where nc = nRows d + 1

fixCycle' :: Eq a => [a] -> (a -> a) -> a -> ([a], [a])
fixCycle' cache f x | x `elem` cache = span (/=x) (reverse cache)
fixCycle' cache f x = fixCycle' (x:cache) f y where y = f x
fixCycle = fixCycle' []

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day14.txt"
    let nrows = length input
    let ncols = length (head input)

    let idx = zip [1..]
    let pegs xs = S.fromList [i | (i, c) <- idx xs, c == '#']
    let ppr = M.fromList [(r, pegs row) | (r, row) <- idx input]
    let ppc = M.fromList [(r, pegs row) | (r, row) <- idx (transpose input)]
    let dish = Dish nrows ncols ppc ppr
    let balls = [(r, c) | (r, row) <- idx input, (c, col) <- idx row, col == 'O']

    print $ dishLoad dish $ rollBalls dish North balls

    let (start', cycle') = fixCycle (rollCycle dish) balls
    let i = (1000000000 - length start') `mod` (length cycle') 
    print $ dishLoad dish $ cycle' !! i
