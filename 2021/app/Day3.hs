module Day3 where

import Control.Monad
import Data.List
import Data.Bits


mostCommon' :: Int -> Int -> String -> Char
mostCommon' zeros ones [] = if zeros > ones then '0' else '1'
mostCommon' zeros ones ('0':xs) = mostCommon' (zeros + 1) ones xs
mostCommon' zeros ones ('1':xs) = mostCommon' zeros (ones + 1) xs
mostCommon' zeros ones (c:xs)   = error $ "Invalid character" ++ show c
mostCommon = mostCommon' 0 0
leastCommon s = if m == '0' then '1' else '0'
    where m = mostCommon s


binToInteger' :: String -> Int -> Int
binToInteger' "" _ = 0
binToInteger' ('0':xs) n  = binToInteger' xs (n - 1)
binToInteger' ('1':xs) n = (2 ^ (n - 1)) + binToInteger' xs (n - 1)
binToInteger' (c:xs)   n = error $ "Invalid character" ++ show c
binToInteger = binToInteger' <*> length


filterNumbers :: (String -> Char) -> Int -> [String] -> [String]
filterNumbers common n xs = filter (\x -> (x !! n) == dig) xs
    where dig = common $ map (!!n) xs


findOxygen :: Int -> [String] -> String
findOxygen _ [x] = x
findOxygen n xs = findOxygen (n + 1) (filterNumbers mostCommon n xs)

findCo2 :: Int -> [String] -> String
findCo2 _ [x] = x
findCo2 n xs = findCo2 (n + 1) (filterNumbers leastCommon n xs)

main :: IO ()
main = do
    numbers <- lines <$!> readFile "data/day3.txt"
    let n = binToInteger $ map mostCommon $ transpose numbers

    print $ n * xor n 31
    let oxygen = binToInteger $ findOxygen 0 numbers
    let co2 = binToInteger $ findCo2 0 numbers
    print $ co2 * oxygen
