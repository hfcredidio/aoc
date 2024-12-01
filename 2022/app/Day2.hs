module Day2 where

import Control.Monad

data Hand = Rock | Paper | Scissors deriving(Eq, Show)
data Result = Win | Draw | Lose
type Game = (Hand, Hand)
type GameResult = (Hand, Result)

instance Ord Hand where
    compare Rock     Paper    = LT
    compare Paper    Scissors = LT
    compare Scissors Rock     = LT
    compare x y | x == y      = EQ
                | otherwise   = GT

parseHand :: Char -> Either String Hand
parseHand 'A' = Right Rock
parseHand 'X' = Right Rock
parseHand 'B' = Right Paper
parseHand 'Y' = Right Paper
parseHand 'C' = Right Scissors
parseHand 'Z' = Right Scissors
parseHand  c  = Left $ "Ivalid hand " ++ show c

parseResult :: Char -> Either String Result
parseResult 'X' = Right Lose
parseResult 'Y' = Right Draw
parseResult 'Z' = Right Win
parseResult  c  = Left $ "Ivalid result " ++ show c

parseGame :: String -> Either String Game
parseGame (p1:' ':p2:_) = do
    h1 <- parseHand p1
    h2 <- parseHand p2
    return (h1, h2)
parseGame g = Left $ "Ivalid game " ++ show g

parseGameResult :: String -> Either String GameResult
parseGameResult (p1:' ':p2:_) = do
    h1 <- parseHand p1
    h2 <- parseResult p2
    return (h1, h2)
parseGameResult g = Left $ "Ivalid game result " ++ show g

handScore :: Hand -> Int
handScore Rock = 1
handScore Paper = 2
handScore Scissors = 3

gameScore :: Game -> Int
gameScore (they, me) = case compare they me of
    GT ->  myScore
    EQ ->  myScore + 3
    LT ->  myScore + 6
    where myScore = handScore me

winningHand :: Hand -> Hand
winningHand Rock = Paper
winningHand Paper = Scissors
winningHand Scissors = Rock

losingHand :: Hand -> Hand
losingHand Rock = Scissors
losingHand Paper = Rock
losingHand Scissors = Paper

gameResultScore :: GameResult -> Int
gameResultScore (h, Win) = gameScore (h, winningHand h)
gameResultScore (h, Draw) = gameScore (h, h)
gameResultScore (h, Lose) = gameScore (h, losingHand h)

main :: IO ()
main = do
    lines <- lines <$!> readFile "data/Day2.txt"
    let games = sequence $ parseGame <$> lines
    print $ sum . map gameScore <$> games

    let gameResults = sequence $ parseGameResult <$> lines
    print $ sum . map gameResultScore <$> gameResults
