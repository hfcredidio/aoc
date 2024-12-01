module Day07 where

import Control.Monad
import Data.List

data Card = C1 | C2 | C3 | C4
          | C5 | C6 | C7 | C8
          | C9 | CT | CJ | CQ
          | CK | CA deriving(Show, Eq, Ord)

newtype Hand = Hand [Card] deriving(Show, Eq)

instance Ord Hand where
    compare h1@(Hand cs1) h2@(Hand cs2) =
        if t1 == t2 then compare cs1 cs2 else compare t1 t2
        where t1 = handType h1
              t2 = handType h2

data HandType = HighC
              | OneP
              | TwoP
              | ThreeOK
              | FullH
              | FourOK
              | FiveOK
              deriving(Show, Eq, Ord)

parseCard :: Char -> Card
parseCard '2' = C2
parseCard '3' = C3
parseCard '4' = C4
parseCard '5' = C5
parseCard '6' = C6
parseCard '7' = C7
parseCard '8' = C8
parseCard '9' = C9
parseCard 'T' = CT
parseCard 'J' = CJ
parseCard 'Q' = CQ
parseCard 'K' = CK
parseCard 'A' = CA
parseCard c = error $ "Invalid card " ++ [c]

parseJoker :: Char -> Card
parseJoker 'J' = C1
parseJoker  c  = parseCard c

handType :: Hand -> HandType
handType (Hand cards) =
    let x = sort . map length . group $ sort $ replaceJoker cards
     in case x of
          [1, 1, 1, 1, 1] -> HighC
          [1, 1, 1, 2]    -> OneP
          [1, 2, 2]       -> TwoP
          [1, 1, 3]       -> ThreeOK
          [2, 3]          -> FullH
          [1, 4]          -> FourOK
          [5]             -> FiveOK
          _ -> error $ "Invalid hand " ++ show cards

mostCommon :: Ord a => [a] -> Maybe a
mostCommon [] = Nothing
mostCommon xs = Just . head . last . sortOn length . group $ sort xs

replaceJoker :: [Card] -> [Card]
replaceJoker cards = case mostCommon $ filter (/=C1) cards of
    Nothing  -> cards
    Just rep -> map (\c -> if c == C1 then rep else c) cards

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day07.txt"
    let hands = Hand . map parseCard . head . words <$> input
    let scores = read . last . words <$> input :: [Int]
    let sortedScores = map snd . sortOn fst $ zip hands scores
    print $ sum $ zipWith (*) [1..] sortedScores

    let jokerHands = Hand . map parseJoker . head . words <$> input
    let sortedScores' = map snd . sortOn fst $ zip jokerHands scores
    print $ sum $ zipWith (*) [1..] sortedScores'
