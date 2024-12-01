module Day01 where

import Control.Monad
import Data.Char
import Data.List

parseDigits :: [Char] -> [Char]
parseDigits [] = []
parseDigits s@(x:xs)
  | isDigit x              =   x:parseDigits xs
  | "one"   `isPrefixOf` s = '1':parseDigits xs
  | "two"   `isPrefixOf` s = '2':parseDigits xs
  | "three" `isPrefixOf` s = '3':parseDigits xs
  | "four"  `isPrefixOf` s = '4':parseDigits xs
  | "five"  `isPrefixOf` s = '5':parseDigits xs
  | "six"   `isPrefixOf` s = '6':parseDigits xs
  | "seven" `isPrefixOf` s = '7':parseDigits xs
  | "eight" `isPrefixOf` s = '8':parseDigits xs
  | "nine"  `isPrefixOf` s = '9':parseDigits xs
  | otherwise              =     parseDigits xs

calibrationValue :: [Char] -> Int
calibrationValue cs = read [head cs, last cs]

main :: IO ()
main = do
    codes <- lines <$!> readFile "data/Day01.txt"
    print $ sum $ calibrationValue . filter isDigit <$> codes
    print $ sum $ calibrationValue . parseDigits <$> codes
