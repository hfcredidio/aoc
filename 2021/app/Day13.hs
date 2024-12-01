module Day13 where

import Control.Monad
import Data.List
import qualified Data.Set as S

type Position = (Int, Int)
data Fold = FoldLeft Int | FoldUp Int deriving(Show)

foldPoint :: Fold -> Position -> S.Set Position
foldPoint (FoldLeft x0) (x, y) = case compare x x0 of
    EQ -> S.empty
    LT -> S.singleton (x, y)
    GT -> S.singleton (x - (x - x0) * 2, y)
foldPoint (FoldUp y0) (x, y) = case compare y y0 of
    EQ -> S.empty
    LT -> S.singleton (x, y)
    GT -> S.singleton (x, y - (y - y0) * 2)


pShow :: S.Set Position -> String
pShow dots = foldl1 (++) $ intersperse "\n" lines
    where (xs, ys) = unzip $ S.toList dots
          (maxX, maxY) = (maximum xs, maximum ys)
          lines = [[if (i, j) `S.member` dots then '#' else ' ' | i <- [0..maxX]] | j <- [0..maxY]]


parsePosition :: String -> Position
parsePosition s = (read a, read b)
    where (a, _:b) = span (/=',') s


parseFold :: String -> Fold
parseFold s = let (a, _:b) = span (/='=') s in case last a of
    'x' -> FoldLeft $ read b
    'y' -> FoldUp $ read b
    _   -> error "Invalid"


parseFile :: String -> (S.Set Position, [Fold])
parseFile s = (positions, folds)
    where (rawPositions, _:rawFolds) = span (/="") $ lines s
          positions = S.fromList $ map parsePosition rawPositions
          folds = map parseFold rawFolds


foldAll :: S.Set Position -> Fold -> S.Set Position
foldAll positions fold = S.foldl S.union S.empty $ S.map (foldPoint fold) positions


main :: IO ()
main = do
    (positions, folds) <- parseFile <$!> readFile "data/day13.txt"
    print $ S.size $ foldAll positions (head folds)
    putStrLn $ pShow $ foldl foldAll positions folds
