module Day04 where

import Control.Monad
import Data.Char (isDigit)
import qualified Data.Set as S

type Numbers = S.Set Int
data Card = Card { cId :: Int
                 , cWinning :: S.Set Int
                 , cNumbers :: S.Set Int
                 , cQuantiy :: Int
                 } deriving(Show, Eq)

parseLine :: String -> (Int, [Int], [Int])
parseLine line = (read cardId, readList wining, readList card)
    where (cid, _:rest) = span (/=':') line
          (wining, _:card) = span (/='|') rest
          cardId = last (words cid)
          readList = map read . words

parseCard :: String -> Card
parseCard s = let (cid, winning, nums) = parseLine s
               in Card { cId = cid
                       , cWinning = S.fromList winning
                       , cNumbers = S.fromList nums
                       , cQuantiy = 1 }
    
cMatches :: Card -> Int
cMatches (Card _ win nums _) = S.size $ (win `S.intersection` nums)

cScore :: Card -> Int
cScore c = case cMatches c of
             0 -> 0
             m -> 2 ^ (m - 1)

updateQuantities :: [Card] -> [Card]
updateQuantities [] = []
updateQuantities (c:cs) = c:cs'
    where m = cMatches c
          q = cQuantiy c
          (a, b) = splitAt m cs
          add1 cc = cc { cQuantiy = cQuantiy cc + q }
          cs' = updateQuantities $ (map add1 a) ++ b


main :: IO ()
main = do
    input <- lines <$!> readFile "./data/Day04.txt"
    print $ sum $ cScore . parseCard <$> input
    print $ sum . map cQuantiy $ updateQuantities $ parseCard <$> input
