module Day03 where

import Debug.Trace
import Control.Monad
import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Data.List
import qualified Data.Matrix as M

data Site = Empty
          | Symbol Char
          | Digit  Char
          deriving(Show)

data NumberMatch = NumberMatch { nRow    :: Int
                               , nSpan   :: (Int, Int)
                               , nDigits :: [Char]
                               } deriving(Show, Ord, Eq)

nValue :: NumberMatch -> Int
nValue (NumberMatch _ _ d) = read d

type Grid = M.Matrix Site

parseSite :: Char -> Site
parseSite  c | c == '.'  = Empty
             | isDigit c = Digit c
             | otherwise = Symbol c

isSymbol :: Site -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isGear :: Site -> Bool
isGear (Symbol '*') = True
isGear _ = False

gridNumbers' :: Int -> Int -> [Site] -> [NumberMatch]
gridNumbers' _ _ [] = []
gridNumbers' r i (Digit d:xs) =
    let ps = gridNumbers' r (i+1) xs
     in case ps of
         (NumberMatch r (start, end) ds):ns | start == i+1 -> (NumberMatch r (i, end) (d:ds)):ns
         _ -> (NumberMatch r (i, i+1) [d]   ):ps
gridNumbers' r i (_:xs) = gridNumbers' r (i+1) xs

gridNumbers :: Int -> [Site] -> [NumberMatch]
gridNumbers r = gridNumbers' r 1

neighbors :: Grid -> Int -> (Int, Int) -> [Site]
neighbors g row (start, end) = map (g M.!) neighs
    where top = [(row-1, c) | c <- [start-1..end]]
          bot = [(row+1, c) | c <- [start-1..end]]
          mid = [(row, start-1), (row, end)]
          inBound (r, c) = 1 <= r && r <= M.nrows g && 1 <= c && c <= M.ncols g
          neighs = filter inBound (top ++ mid ++ bot)

isPartNumber :: Grid -> NumberMatch -> Bool
isPartNumber grid (NumberMatch row span _) = any isSymbol ns
    where ns = neighbors grid row span

gearRatio :: [NumberMatch] -> (Int, Int) -> Maybe Int
gearRatio ns (r, c) = case matches of
    [a, b] -> Just (a * b)
    _      -> Nothing
    where neigh = [ (r-i,c-j) | i <- [-1..1], j <- [-1..1] ]
          isMatch (NumberMatch r' (c0, c1) _) (r, c) = r == r' && c0 <= c && c < c1
          matches = [ nValue num | num <- ns, any (isMatch num) neigh ]

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day03.txt"

    let nrows = length input
    let ncols = length $ head input
    let grid  = M.fromList nrows ncols $ parseSite <$> concat input

    let partNumbers = [ num
                      | (r, row) <- zip [1..] $ M.toLists grid
                      , num <- gridNumbers r row
                      , isPartNumber grid num ]

    let gearPositions = [ (r, c)
                        | r <- [1..nrows]
                        , c <- [1..nrows]
                        , isGear (grid M.! (r, c)) ]

    print $ sum $ map nValue partNumbers
    print $ sum . catMaybes $ map (gearRatio partNumbers) gearPositions
