module Day07 where

import Control.Monad

parseEquation :: String -> (Int, [Int])
parseEquation s = let (test, _:values) = span (/=':') s
                   in (read test, map read $ words values)

-- evals from right to left
allEvals' :: [Int -> Int -> Int] -> [Int] -> [Int]
allEvals' ops [] = []
allEvals' ops [x] = [x]
allEvals' ops (x:xs) = let res = allEvals' ops xs
                       in ops <*> [x] <*>  res

-- evals from left to right
allEvals ops xs = allEvals' (map flip ops) (reverse xs)

isCalibrated :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
isCalibrated ops test vals = test `elem` allEvals ops vals

(|||) :: Int -> Int -> Int
x ||| y = x * (10^n) + y
    where n = floor $ logBase 10 (fromIntegral y) + 1

main :: IO ()
main = do
    equations <- map parseEquation . lines <$!> readFile "data/Day07.txt"
    print $ sum [ test 
                | (test, vals) <- equations
                , isCalibrated [(+), (*)] test vals ]
    print $ sum [ test 
                | (test, vals) <- equations
                , isCalibrated [(+), (*), (|||)] test vals ]
