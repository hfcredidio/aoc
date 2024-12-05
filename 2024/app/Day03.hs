module Day03 where

import Data.List (uncons)
import Control.Monad
import Data.Char

parseNum :: String -> Maybe (Int, String)
parseNum s = case span isDigit s of
    ("", _) -> Nothing
    (a, b)  -> Just (read a, b)

parseNums :: String -> Maybe ((Int, Int), String)
parseNums xs = do
    (lhs, xs') <- parseNum xs
    (comma, xs'') <- uncons xs'
    guard $ comma == ','
    (rhs, xs''') <- parseNum xs''
    return ((lhs, rhs), xs''')

parseMul :: String -> Maybe ((Int, Int), String)
parseMul ('m':'u':'l':'(':xs) = do
    (nums, xs') <- parseNums xs
    (comma, xs'') <- uncons xs'
    guard $ comma == ')'
    return (nums, xs'')
parseMul _ = Nothing

parseMuls :: String -> [(Int, Int)]
parseMuls [] = []
parseMuls xs = case parseMul xs of
    Nothing -> parseMuls (tail xs)
    Just (mul, xs') -> mul:parseMuls xs'

parseDo :: String -> String
parseDo [] = []
parseDo ('d':'o':'(':')':xs) = xs
parseDo (_:xs) = parseDo xs

parseCond :: String -> String
parseCond ('d':'o':'n':'\'':'t':'(':')':xs) = parseDo xs
parseCond xs = xs

parseMulsCond :: String -> [(Int, Int)]
parseMulsCond [] = []
parseMulsCond xs = 
    let xs' = parseCond xs
     in case parseMul xs' of
        Nothing          -> if null xs' then [] else parseMulsCond (tail xs')
        Just (mul, xs'') -> mul:parseMulsCond xs''

main :: IO ()
main = do
    memory <- readFile "data/Day03.txt"
    print $ sum . map (uncurry (*)) $ parseMuls memory
    print $ sum . map (uncurry (*)) $ parseMulsCond memory
