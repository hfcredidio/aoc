module Day4 where

import Control.Monad
import Data.List

data Entry = Entry { entryValue :: Int
                   , entryMarked :: Bool
                   } deriving(Show)

type Board = [[Entry]]

boardRows :: Board -> [[Entry]]
boardRows = id

boardCols :: Board -> [[Entry]]
boardCols = transpose

boardHasWon :: Board -> Bool
boardHasWon b = any (all entryMarked) (boardRows b) || any (all entryMarked) (boardCols b)

boardScore :: Int -> Board -> Int
boardScore lastCall b = lastCall * sum [entryValue e | row <- boardRows b, e <- row, (not . entryMarked) e]

markEntry :: Entry -> Entry
markEntry e = Entry (entryValue e) True

markRow :: Int -> [Entry] -> [Entry]
markRow value row = let (h, t) = span (\e -> value /= entryValue e) row in
    case t of
        [] -> h
        _  -> h ++ markEntry (head t):tail t

markBoard :: Int -> Board -> Board
markBoard value b = map (markRow value) (boardRows b)

findWinner :: [Int] -> [Board] -> Either String (Int, Board)
findWinner [] boards = Left "No calls left and no winners????"
findWinner (call:cs) boards = let markedBoards = map (markBoard call) boards in
    case filter boardHasWon markedBoards of
        []        -> findWinner cs markedBoards
        [winner]  -> Right (call, winner)
        _         -> Left "Too many winners????"

findLastWinner :: [Int] -> [Board] -> Either String (Int, Board)
findLastWinner [] [] = Left "What a weird situation."
findLastWinner [] boards = Left "No calls left and still some unwinning boards."
findLastWinner (call:cs) [board] = let markedBoard = markBoard call board in
    if boardHasWon markedBoard then
        Right (call, markedBoard) 
    else
        findLastWinner cs [markedBoard]
    
findLastWinner (call:cs) boards = findLastWinner cs $ filter (not . boardHasWon) markedBoards
    where markedBoards = map (markBoard call) boards

parseList :: Char -> String -> [Int]
parseList _ "" = []
parseList sep s = read h:parseList sep t
    where (h, t) = span (/=sep) (dropWhile (==sep) s)

parseBoard :: [String] -> Board
parseBoard ss = [[Entry v False | v <- row] | row <- values]
    where values = map (parseList ' ') ss

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards ss = parseBoard h:parseBoards t
    where (h, t) = span (/="") (dropWhile (=="") ss)


main :: IO ()
main = do
    inputs <- lines <$!> readFile "data/day4.txt"
    let calls  = parseList ',' (head inputs)
    let boards = parseBoards (drop 2 inputs)
    print $ uncurry boardScore <$> findWinner calls boards
    print $ uncurry boardScore <$> findLastWinner calls boards
