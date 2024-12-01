module Day18 where

import Control.Monad
import Data.List
import qualified Data.Set as S
import qualified Data.Matrix as M

type Coord = (Int, Int)
data Direction = U | D | L | R deriving(Show, Eq, Enum)
data Trench = Trench { dDirection :: Direction
                     , dDistance  :: Int
                     } deriving(Show)

parseDirection :: String -> Direction
parseDirection "L" = L
parseDirection "R" = R
parseDirection "U" = U
parseDirection "D" = D
parseDirection  s  = error $ "Invalid direction " ++ s

parseTrench :: String -> Trench
parseTrench s = case words s of
    [dir, dist, color] -> Trench (parseDirection dir) (read dist)
    _ -> error $ "Invalid trench " ++ s

parseTrench' :: String -> Trench
parseTrench' s = Trench direction distance
    where s' = init $ tail $ dropWhile (/='#') s
          distance = read $ "0x" ++ (init s')
          direction = case last s' of
            '0' -> R
            '1' -> D
            '2' -> L
            '3' -> U
            c   -> error $ "Invalid Direction" ++ [c]

isInternal :: Direction -> Direction -> Bool
isInternal R D = False
isInternal D L = False
isInternal L U = False
isInternal U R = False
isInternal D R = True
isInternal L D = True
isInternal U L = True
isInternal R U = True
isInternal _ _ = error "Invalid Corner"

areInternal :: [Direction] -> [Bool]
areInternal [] = []
areInternal ds = map (uncurry isInternal) ds'
    where ds' = zip ds (tail $ cycle ds)

endVertex :: Coord -> Trench -> Coord
endVertex (r, c) (Trench U d) = (r - d, c)
endVertex (r, c) (Trench D d) = (r + d, c)
endVertex (r, c) (Trench R d) = (r, c + d)
endVertex (r, c) (Trench L d) = (r, c - d)

vertices' :: Coord -> [Trench] -> [Coord]
vertices' start [] = []
vertices' start (v:vs) = start:vertices' v' vs
    where v' = endVertex start v

vertices = vertices' (0, 0)

shoelace :: [Coord] -> Int
shoelace xs = (sum $ map shoelace' xs') `div` 2
    where xs' = zip xs (tail $ cycle xs)
          shoelace' ((r, c), (r', c')) = (r' * c) - (r * c')

area :: [Trench] -> Int
area ts = area1 + area2 + (area3 `div` 2)
    where area1 = shoelace $ vertices ts
          isInt = areInternal $ map dDirection ts
          internalCorners = sum $ map fromEnum isInt
          externalCorners = sum $ map (fromEnum . not) isInt
          area2 = (internalCorners + 3 * externalCorners) `div` 4
          area3 = sum $ map (\t -> dDistance t - 1) ts

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day18.txt"
    print $ area $ parseTrench  <$> input
    print $ area $ parseTrench' <$> input
