module Day11 where

import qualified Data.Map as M
import Control.Monad

splitInt :: Int -> Maybe (Int, Int)
splitInt n = let ndigs = floor (logBase 10 (fromIntegral n)) + 1
                 pow10 = 10^(ndigs `div` 2)
              in if odd ndigs
              then Nothing
              else Just (n `div` pow10, n `mod` pow10)

type Cache = M.Map (Int, Int) Int

countStones :: Int -> Int -> Cache -> (Int, Cache)
countStones steps value cache | M.member (steps, value) cache = (cache M.! (steps, value), cache)
countStones 0 value cache = (1, cache)
countStones steps 0 cache = countStones (steps - 1) 1 cache
countStones steps value cache = case splitInt value of
    Nothing -> let (c, cache') = countStones (steps - 1) (value * 2024) cache
                in (c, M.insert (steps, value) c cache')
    Just (left, right) -> let (c, cache') = countStones (steps - 1) left cache
                              (c', cache'') = countStones (steps - 1) right cache'
                              c'' = c + c'
                           in (c'', M.insert (steps, value) c'' cache'')

main :: IO ()
main = do
    values <- map read . words <$!> readFile "data/Day11.txt" :: IO [Int]
    print $ sum . map fst $ scanr (\value (_, cache) -> countStones 25 value cache) (0, M.empty) values
    print $ sum . map fst $ scanr (\value (_, cache) -> countStones 75 value cache) (0, M.empty) values
