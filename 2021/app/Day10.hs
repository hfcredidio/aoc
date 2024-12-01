module Day10 where

import Control.Monad
import Data.List

data Bracket = Square | Curly | Round | Angle deriving(Eq, Show)
data Delimiter a = L a | R a deriving(Show)

parseDelimiter :: Char -> Delimiter Bracket
parseDelimiter '{' = L Curly
parseDelimiter '[' = L Square
parseDelimiter '(' = L Round
parseDelimiter '<' = L Angle
parseDelimiter '}' = R Curly
parseDelimiter ']' = R Square
parseDelimiter ')' = R Round
parseDelimiter '>' = R Angle
parseDelimiter c   = error $ "Invalid character " ++ show c

data CheckedLine = Corrupt Bracket | Incomplete [Bracket] | Correct deriving(Show)

isCorrupt :: CheckedLine -> Bool
isCorrupt (Corrupt _) = True
isCorrupt _           = False


isIncomplete :: CheckedLine -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _              = False


checkLine' :: [Bracket] -> [Delimiter Bracket] -> CheckedLine
checkLine' [] [] = Correct
checkLine' stack [] = Incomplete $ reverse stack
checkLine' stack (L b:bs) = checkLine' (b:stack) bs
checkLine' stack (R b:bs) = case stack of
     (s:ss) | s == b -> checkLine' ss bs
     (s:ss) | s /= b -> Corrupt b
     []              -> Corrupt b
     _               -> error "I've no idea"
checkLine = checkLine' []


score :: CheckedLine -> Int
score Correct = 0
score (Corrupt Round)  = 3
score (Corrupt Square) = 57
score (Corrupt Curly)  = 1197
score (Corrupt Angle)  = 25137
score (Incomplete (Round:bs))  = 1 + 5 * score (Incomplete bs)
score (Incomplete (Square:bs)) = 2 + 5 * score (Incomplete bs)
score (Incomplete (Curly:bs))  = 3 + 5 * score (Incomplete bs)
score (Incomplete (Angle:bs))  = 4 + 5 * score (Incomplete bs)
score (Incomplete []) = 0


main :: IO ()
main = do
    checkedChunks <- map (checkLine . map parseDelimiter) . lines <$!> readFile "data/day10.txt"
    print $ sum $ map score $ filter isCorrupt checkedChunks
    let sortedIncompleteScores = sort $ map score $ filter isIncomplete checkedChunks
    print $ sortedIncompleteScores !! (length sortedIncompleteScores `div` 2)
