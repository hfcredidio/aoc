module Day10 where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Matrix as M

data Site = SiteTB
          | SiteLR
          | SiteTL
          | SiteTR
          | SiteBL
          | SiteBR
          | SiteStart
          | SiteEmpty deriving(Show, Eq)

parseSite :: Char -> Site
parseSite '|' = SiteTB
parseSite '-' = SiteLR
parseSite 'L' = SiteTR
parseSite 'J' = SiteTL
parseSite '7' = SiteBL
parseSite 'F' = SiteBR
parseSite '.' = SiteEmpty
parseSite 'S' = SiteStart
parseSite  c  = error $ "Invalid site " ++ [c]

type Grid = M.Matrix Site
data Direction = U | D | L | R deriving(Show)
data Position = Position Int Int Direction deriving(Show)

pCoords :: Position -> (Int, Int)
pCoords (Position r c _) = (r, c)

step' :: Grid -> Position -> (Position -> Maybe Position)
step' g p@(Position r c d) = case (M.getElem r c g, d) of
    (SiteTB, U) ->  moveUp
    (SiteTB, D) ->  moveDown
    (SiteLR, R) ->  moveRight
    (SiteLR, L) ->  moveLeft
    (SiteTR, D) ->  moveRight
    (SiteTR, L) ->  moveUp
    (SiteTL, D) ->  moveLeft
    (SiteTL, R) ->  moveUp
    (SiteBR, U) ->  moveRight
    (SiteBR, L) ->  moveDown
    (SiteBL, U) ->  moveLeft
    (SiteBL, R) ->  moveDown
    (SiteStart, R) ->  moveRight
    (SiteStart, L) ->  moveLeft
    (SiteStart, U) ->  moveUp
    (SiteStart, D) ->  moveDown
    _ -> const Nothing
    where moveRight (Position r c _) | c == M.ncols g = Nothing
          moveRight (Position r c _) = Just $ Position r (c+1) R

          moveLeft  (Position r 1 _) = Nothing
          moveLeft  (Position r c _) = Just $ Position r (c-1) L

          moveUp    (Position 1 c _) = Nothing
          moveUp    (Position r c _) = Just $ Position (r-1) c U

          moveDown  (Position r c _) | r == M.nrows g = Nothing
          moveDown  (Position r c _) = Just $ Position (r+1) c D

step g p = step' g p p

startCoords :: Grid -> (Int, Int)
startCoords g = head [ (r, c) |
                       r <- [1..M.nrows g]
                     , c <- [1..M.ncols g]
                     , M.getElem r c g == SiteStart
                     ]

startDirection :: Grid -> (Int, Int) -> (Site, Direction)
startDirection g (r, c) =
    case (isUp, isDown, isLeft, isRight) of
      (True, True, _, _) -> (SiteTB, U)
      (True, _, True, _) -> (SiteTL, U)
      (True, _, _, True) -> (SiteTR, U)
      (_, True, True, _) -> (SiteBL, D)
      (_, True, _, True) -> (SiteBR, D)
      (_, _, True, True) -> (SiteBL, L)
      _ -> error "BV"
    where peek (r, c) cs = M.safeGet r c g `elem` (Just <$> cs)
          isUp    = peek (r-1, c) [SiteBR, SiteBL, SiteTB]
          isDown  = peek (r+1, c) [SiteTR, SiteTL, SiteTB]
          isLeft  = peek (r, c-1) [SiteTR, SiteBR, SiteLR]
          isRight = peek (r, c+1) [SiteTL, SiteBL, SiteLR]

startPosition :: Grid -> Position
startPosition g = let (r, c) = startCoords g
                   in Position r c (snd $ startDirection g (r, c))

getLoop :: Grid -> Maybe [Position]
getLoop g = sequenceWhile (not . isStart) positions
    where startPos = startPosition g
          positions = drop 1 $ iterate (step g =<<) (Just startPos)
          isStart p = pCoords p == pCoords startPos

sequenceWhile :: (a -> Bool) -> [Maybe a] -> Maybe [a]
sequenceWhile pred [] = Just []
sequenceWhile pred (Nothing:xs) = Nothing
sequenceWhile pred (Just x:xs) | pred x = (x:) <$> sequenceWhile pred xs
                               | otherwise = Just []

show' :: Grid -> String
show' g = concat [showRow r | r <- [1..M.nrows g]]
    where show'' SiteStart = 'S'
          show'' SiteTB = '║'
          show'' SiteLR = '═'
          show'' SiteTL = '╝'
          show'' SiteTR = '╚'
          show'' SiteBL = '╗'
          show'' SiteBR = '╔'
          show'' SiteEmpty = ' '
          showRow r = '\n':[show'' (M.getElem r c g) | c <- [1..M.ncols g]]

insideSites :: [Site] -> Int
insideSites sites =
    length $ filter isInside' $ zip sites (crosses sites)
    where crossVal SiteTR = 1/2
          crossVal SiteTL = -1/2
          crossVal SiteBR = -1/2
          crossVal SiteBL = 1/2
          crossVal SiteLR = 0
          crossVal SiteTB = 1
          crossVal SiteEmpty = 0
          crossVal SiteStart = undefined
          crosses sites = scanl (+) 0.0 $ map crossVal sites
          isInside' (s, c) = isInside c && s == SiteEmpty

isIntegral :: Double -> Bool
isIntegral x = x == fromIntegral (floor x)

isInside :: Double -> Bool
isInside x = isIntegral x && odd (floor x)

eraseUseless' :: Grid -> Grid -> [Position] -> Grid
eraseUseless' acc g [] = M.setElem typ (r, c) acc
    where (r, c) = startCoords g
          (typ, _) = startDirection g (r, c)
eraseUseless' acc g (Position r c _:xs) = eraseUseless' acc' g xs
    where site = M.getElem r c g
          acc' = M.setElem site (r, c) acc

eraseUseless g = eraseUseless' acc g
    where nrows = M.nrows g
          ncols = M.ncols g
          acc = M.matrix nrows ncols (const SiteEmpty)

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day10.txt"
    let nrows = length input
    let ncols = length $ head input
    let grid = M.fromList nrows ncols (map parseSite $ concat input)
    let loop = fromJust $ getLoop grid
    putStrLn $ show' grid

    let loopLength = length loop
    print $ (loopLength + 1) `div` 2

    let newGrid = eraseUseless grid loop
    putStrLn $ show' newGrid
    print $ sum $ insideSites <$>  M.toLists newGrid
