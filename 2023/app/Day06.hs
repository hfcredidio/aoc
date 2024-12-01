module Day06 where

import Control.Monad
import Data.Char

data Quadratic = Quadratic Double Double Double deriving(Show)

roots :: Quadratic -> (Double, Double)
roots (Quadratic a b c) = ((-b-d)/(2*a), (-b+d)/(2*a))
    where d = sqrt $ b*b - 4*a*c

integerCount :: (Double, Double) -> Int
integerCount (a, b) | a > b     = integerCount (b, a)
                    | otherwise = ceiling (b - 1) - floor (a + 1) + 1

mkQuadratic :: Double -> Double -> Quadratic
mkQuadratic duration record = Quadratic (-1.0) duration (-record)

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day06.txt"
    let durations = map read $ tail $ words $ head input :: [Double]
    let records = map read $ tail $ words $ last input :: [Double]
    print $ foldl (*) 1 $ integerCount <$> roots <$> zipWith mkQuadratic durations records

    let duration = read $ filter isDigit $ head input :: Double
    let record = read $ filter isDigit $ last input :: Double
    print $ integerCount . roots $ mkQuadratic duration record
