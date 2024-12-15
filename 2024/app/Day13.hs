module Day13 where

import Control.Monad
import Data.Maybe (mapMaybe)

data Line = Line Double Double

mkLine :: (Double, Double) -> (Double, Double) -> Line
mkLine (x0, y0) (x1, y1) = Line b m 
    where m = (x1 - x0) / (y1 - y0)
          b = x0 - m * y0

map2 :: (a -> b) -> (a, a) -> (b, b)
map2 f (x, y) = (f x, f y)

mkLineI :: (Int, Int) -> (Int, Int) -> Line
mkLineI p0 p1 = mkLine (map2 fromIntegral p0) (map2 fromIntegral p1)

lineInter :: Line -> Line -> (Double, Double)
lineInter (Line b1 m1) (Line b2 m2) = (x, y)
    where y = (b2 - b1) / (m1 - m2)
          x = m1 * (b2 - b1) / (m1 - m2) + b1

solution :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
solution (xA, yA) (xB, yB) (xP, yP) =
    if xA * aCount + xB * bCount == xP
        then Just (aCount, bCount)
        else Nothing
    where l1 = mkLineI (0, 0) (xA, yA)
          l2 = mkLineI (xP, yP) (xP+xB, yP+yB)
          (x, y) = map2 round $ lineInter l1 l2
          aCount = x `div` xA
          bCount = (xP - x) `div` xB

tokens :: (Int, Int) -> Int
tokens (aCount, bCount) = 3 * aCount + bCount

parseLine :: String -> (Int, Int)
parseLine line = case words line of
    ["Button", "A:", 'X':'+':xs, 'Y':'+':ys] -> (read (init xs), read ys)
    ["Button", "B:", 'X':'+':xs, 'Y':'+':ys] -> (read (init xs), read ys)
    ["Prize:", 'X':'=':xs, 'Y':'=':ys] -> (read (init xs), read ys)
    _ -> error "Invalid"

parseArgs :: [String] -> ((Int, Int), (Int, Int), (Int, Int))
parseArgs [a, b, c] = (parseLine a, parseLine b, parseLine c)
parseArgs _ = error "Invalid"


split :: Int -> [a] -> [[a]]
split chunk [] = []
split chunk xs = take chunk xs:split chunk (drop chunk xs)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

minimum' [] = 0
minimum' xs = minimum xs

correctInput :: (a, b, (Int, Int)) -> (a, b, (Int, Int))
correctInput (a, b, (x, y)) = (a, b, (x+10000000000000, y+10000000000000))

main :: IO ()
main = do
    lines <- lines <$!> readFile "data/Day13.txt"
    let inputs = map parseArgs . split 3 . filter (/="") $ lines
    print $ sum . map tokens $ mapMaybe (uncurry3 solution) inputs
    print $ sum . map tokens $ mapMaybe (uncurry3 solution . correctInput) inputs
