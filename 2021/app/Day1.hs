module Day1 where


import Control.Monad
import Data.List


parseLines :: String -> [Int]
parseLines = map read . lines


countIncreases :: [Int] -> Int
countIncreases = length . filter (>0) . diffs
    where diffs = zipWith (flip (-)) <*> tail


rollingWindow :: Int -> [a] -> [[a]]
rollingWindow n = map (take n) . takeWhile bigEnough . tails 
    where bigEnough x = length x >= n


main :: IO ()
main = do
    numbers <- parseLines <$!> readFile "data/day1.txt"

    putStr "Number of increases: "
    print $ countIncreases numbers

    putStr "Number of increases after a rolling sum: "
    let windowSum = map sum $ rollingWindow 3 numbers
    print $ countIncreases windowSum
