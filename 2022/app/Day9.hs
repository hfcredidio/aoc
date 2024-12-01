module Day9 where

import Control.Monad
import qualified Data.Set as S

type Position = (Int, Int)
data Rope = Knot { rPos :: Position, rTail :: Rope }
          | Tail { rPos :: Position } deriving(Show)
data Direction = U | D | L | R deriving(Show)

dirFromStr :: String -> Direction
dirFromStr "U" = U
dirFromStr "D" = D
dirFromStr "L" = L
dirFromStr "R" = R
dirFromStr  _  = undefined

mkRope :: Int -> Rope
mkRope 1 = Tail (0, 0)
mkRope length = iterate (Knot (0, 0)) (mkRope 1) !! (length - 1)

tailPos :: Rope -> Position
tailPos (Tail p) = p
tailPos (Knot _ t) = tailPos t

followHead :: Position -> Position -> Position
followHead (hx, hy) (tx, ty) = if sqrDist <= 2
                             then (tx, ty)
                             else (tx + signum (hx - tx), ty + signum (hy - ty))
    where sqrDist = (tx - hx) ^ 2 + (ty - hy) ^ 2

moveRope' :: Position -> Rope -> Rope
moveRope' newPos (Tail _) = Tail newPos
moveRope' newHead (Knot oldHead t) = Knot newHead newTail
    where oldTail = rPos t
          newTail = moveRope' (followHead newHead oldTail) t

moveRope :: Direction -> Rope -> Rope
moveRope dir rope = case dir of
    U -> moveRope' (x, y + 1) rope
    D -> moveRope' (x, y - 1) rope
    L -> moveRope' (x - 1, y) rope
    R -> moveRope' (x + 1, y) rope
    where (x, y) = rPos rope

parseLine :: String -> [Direction]
parseLine s = case words s of
    [dir, n] -> replicate (read n) (dirFromStr dir)
    _        -> undefined

main :: IO ()
main = do
    moves <- reverse . concatMap parseLine . lines <$!> readFile "data/Day9.txt"

    let hist = scanr moveRope (mkRope 2) moves
    print $ S.size $ S.fromList $ tailPos <$> hist

    let hist = scanr moveRope (mkRope 10) moves
    print $ S.size $ S.fromList $ tailPos <$> hist
