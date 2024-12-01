module Day13 where

import Control.Monad
import Data.List

paragraphs :: [String] -> [[String]]
paragraphs [] = []
paragraphs ("":ss) = paragraphs ss
paragraphs ss = g:paragraphs rest
    where (g, rest) = span (/="") ss

mirrorIdx' :: Eq a => ([[a]] -> [[a]] -> Bool) -> [[a]] -> [[a]] -> Int
mirrorIdx' cmp []     [] = undefined
mirrorIdx' cmp [] (y:ys) = mirrorIdx' cmp [y] ys
mirrorIdx' cmp xs     [] = 0
mirrorIdx' cmp xs ys | xs `cmp` ys = length xs
                     | ys `cmp` xs = length xs
                     | otherwise   = mirrorIdx' cmp (head ys:xs) (tail ys)

isAlmostPrefixOf :: Eq a => [[a]] -> [[a]] -> Bool
isAlmostPrefixOf [] [] = False
isAlmostPrefixOf xs [] = False
isAlmostPrefixOf [] ys = False
isAlmostPrefixOf (x:xs) (y:ys)
  | x == y        = isAlmostPrefixOf xs ys
  | diffByOne x y = xs `isPrefixOf` ys
  | otherwise     = False
  where diffByOne x y = sum (fromEnum <$> zipWith (/=) x y) == 1

mirrorIdx  = mirrorIdx' isPrefixOf []
mirrorIdx2 = mirrorIdx' isAlmostPrefixOf []

main :: IO ()
main = do
    input <- paragraphs . lines <$!> readFile "data/Day13.txt"
    let hor = sum $ map mirrorIdx input
    let ver = sum $ map (mirrorIdx . transpose) input
    print $ 100 * hor + ver

    let hor = sum $ map mirrorIdx2 input
    let ver = sum $ map (mirrorIdx2 . transpose) input
    print $ 100 * hor + ver
