module Day02 where

import Control.Monad
import Data.List

data Pull = Pull { pRed   :: Int
                 , pGreen :: Int
                 , pBlue  :: Int
                 } deriving (Show)

pPower :: Pull -> Int
pPower (Pull r g b) = r * g * b

pIsValid :: Pull -> Bool
pIsValid (Pull r g b) = r <= 12 && g <= 13 && b <= 14

data Game = Game { gId    :: Int
                 , gPulls :: [Pull]
                 } deriving (Show)

gMinSet :: Game -> Pull
gMinSet (Game _ pulls) = case maximum <$> (map <$> [pRed, pGreen, pBlue] <*> [pulls]) of
    [r, g, b] -> Pull r g b
    _ -> undefined

gIsValid :: Game -> Bool
gIsValid (Game _ pulls) = all pIsValid pulls


-- Parsing functions
splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen pred arr = case findIndex pred arr of
                       Nothing -> (arr, [])
                       Just i  -> case splitAt i arr of
                                    (a, []) -> (a, [])
                                    (a, _:bs) -> (a, bs)

splitAllWhen :: (a -> Bool) -> [a] -> [[a]]
splitAllWhen pred []  = []
splitAllWhen pred arr = let (a, b) = splitWhen pred arr
                        in a:splitAllWhen pred b

parsePull' :: Pull -> String -> Pull
parsePull' pull "" = pull
parsePull' (Pull r g b) s =
    let (first, rest) = splitWhen (==',') s
    in case words first of
         [count,   "red"] -> parsePull' (Pull (read count) g b) rest
         [count, "green"] -> parsePull' (Pull r (read count) b) rest
         [count,  "blue"] -> parsePull' (Pull r g (read count)) rest
         _                -> error $ "Invalid pull " ++ s
parsePull = parsePull' (Pull 0 0 0)

parsePulls :: String -> [Pull]
parsePulls s = map parsePull $ splitAllWhen (==';') s

parseGame :: String -> Game
parseGame s = let (a, b) = splitWhen (==':') s
              in case words a of
                   ["Game", id] -> Game (read id) (parsePulls b)
                   _            -> error $ "Invalid game " ++ s

main :: IO ()
main = do
    lines <- lines <$!> readFile "data/Day02.txt"
    let games = parseGame <$> lines
    print $ sum $ map gId $ filter gIsValid games
    print $ sum $ pPower . gMinSet <$> games
