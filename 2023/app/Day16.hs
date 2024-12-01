module Day16 where

import Data.List
import Debug.Trace
import Control.Monad
import qualified Data.Matrix as M
import qualified Data.Set as S

data Site = Empty
          | RMirror
          | LMirror
          | VSplitter
          | HSplitter
          deriving(Show, Eq)

type Grid = M.Matrix Site
data Direction = U | D | L | R deriving(Show, Eq, Ord)
data BeamS = BeamS Int Int Direction deriving(Show, Eq, Ord)

parseSite :: Char -> Site
parseSite '.'  = Empty
parseSite '/'  = RMirror
parseSite '\\' = LMirror
parseSite '|'  = VSplitter
parseSite '-'  = HSplitter
parseSite  c = error $ "Invalid site " ++ [c]

inBounds :: Grid -> BeamS -> Bool
inBounds g (BeamS r c _) = inR && inC
    where nrows = M.nrows g
          ncols = M.ncols g
          inR = 1 <= r && r <= nrows
          inC = 1 <= c && c <= ncols

moveBeam :: BeamS -> BeamS
moveBeam (BeamS r c U) = BeamS (r - 1) c U
moveBeam (BeamS r c D) = BeamS (r + 1) c D
moveBeam (BeamS r c L) = BeamS r (c - 1) L
moveBeam (BeamS r c R) = BeamS r (c + 1) R

updateBeamS :: Site -> BeamS -> [BeamS]
updateBeamS Empty bs = [moveBeam bs]
updateBeamS RMirror (BeamS r c d) = moveBeam <$> case d of
    U -> [BeamS r c R]
    D -> [BeamS r c L]
    R -> [BeamS r c U]
    L -> [BeamS r c D]
updateBeamS LMirror (BeamS r c d) = moveBeam <$> case d of
    U -> [BeamS r c L]
    D -> [BeamS r c R]
    R -> [BeamS r c D]
    L -> [BeamS r c U]
updateBeamS HSplitter (BeamS r c d) = moveBeam <$> case d of
    U -> [BeamS r c L, BeamS r c R]
    D -> [BeamS r c L, BeamS r c R]
    L -> [BeamS r c L]
    R -> [BeamS r c R]
updateBeamS VSplitter (BeamS r c d) = moveBeam <$> case d of
    U -> [BeamS r c U]
    D -> [BeamS r c D]
    L -> [BeamS r c D, BeamS r c U]
    R -> [BeamS r c D, BeamS r c U]

beams :: S.Set BeamS -> Grid -> BeamS -> S.Set BeamS
beams acc g b | b `S.member` acc = acc
beams acc g b@(BeamS r c _) = acc'
    where s = g M.! (r, c)
          bs = filter (inBounds g) (updateBeamS s b)
          acc' = foldl (\c -> beams c g) (S.insert b acc) bs

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day16.txt"
    let nrows = length input
    let ncols = length (head input)
    let grid =  M.fromList nrows ncols $ map parseSite (concat input)
    let bs = beams S.empty grid (BeamS 1 1 R)
    let part1 start = length $ nub $ map (\(BeamS r c _) -> (r, c)) $ S.toList $ beams S.empty grid start

    let starts = concat [ [(BeamS 1 c D) | c <- [1..ncols]]
                        , [(BeamS nrows c U) | c <- [1..ncols]]
                        , [(BeamS r 1 R) | r <- [1..nrows]]
                        , [(BeamS r ncols R) | r <- [1..nrows]]
                        ]
    print $ maximum $ map part1 starts
