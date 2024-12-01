module Day01 where

import Data.List
import Control.Monad
import qualified Data.Map.Strict as M

readNumbers :: String -> (Int, Int)
readNumbers line = case words line of
    [left, right] -> (read left, read right)
    _ -> error "Malformed line"

listDistance :: [Int] -> [Int] -> Int
listDistance left right = sum . map abs $ zipWith (-) (sort left) (sort right)

countValues :: Ord a => [a] -> M.Map a Int
countValues = foldl updateCounter M.empty
    where updateCounter partial val = M.insertWith (+) val 1 partial

similarityScore :: [Int] -> [Int] -> Int
similarityScore left right = sum $ map score left
    where rightCount = countValues right
          score val = val * M.findWithDefault 0 val rightCount

main :: IO ()
main = do
    (left, right) <- unzip . map readNumbers . lines <$!> readFile "data/Day01.txt"
    print $ listDistance left right
    print $ similarityScore left right
