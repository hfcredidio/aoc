module Day02 where

import Control.Monad

readInts :: [Char] -> [Int]
readInts = map read . words

isMonotonic :: [Int] -> Bool
isMonotonic (x:y:z:xs) 
    | x < y && y < z = isMonotonic (y:z:xs) 
    | x > y && y > z = isMonotonic (y:z:xs) 
    | otherwise      = False
isMonotonic _ = True

isSafe :: [Int] -> Bool
isSafe xs = isMonotonic xs && (maxStep `elem` [1, 2, 3])
    where maxStep = maximum $ abs <$> zipWith (-) (tail xs) xs

popEach :: [a] -> [[a]]
popEach [] = []
popEach (x:xs) = xs:map (x:) (popEach xs)

isSafeWithRemoval :: [Int] -> Bool
isSafeWithRemoval xs = isSafe xs || any isSafe (popEach xs)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter id . map f

main :: IO ()
main = do
    reports <- map readInts . lines <$!> readFile "data/Day02.txt"
    print $ count isSafe reports
    print $ count isSafeWithRemoval reports
