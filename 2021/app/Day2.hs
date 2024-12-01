module Day2 where


import Control.Monad
import Data.List


data Action = Forward Int | Up Int | Down Int deriving(Show)


splitLine :: String -> [String]
splitLine "" = []
splitLine s = a:splitLine (dropWhile (==' ') b)
    where (a, b) = span (/=' ') s

    
parseLine :: String -> Either String Action
parseLine s = case splitLine s of
    ["up", x]      -> Right $ Up (read x)
    ["down", x]    -> Right $ Down (read x)
    ["forward", x] -> Right $ Forward (read x)
    _              -> Left $ "Invalid command: " ++ s


parseLines :: String -> Either String [Action]
parseLines = mapM parseLine . lines


class Submarine s where
    moveSubmarine :: s -> Action -> s
    position :: s -> (Int, Int)


data ShittySubmarine = ShittySubmarine Int Int deriving(Show)
instance Submarine ShittySubmarine where
    moveSubmarine (ShittySubmarine x y) (Forward i) = ShittySubmarine (x + i) y
    moveSubmarine (ShittySubmarine x y) (Up i) = ShittySubmarine x (y + i)
    moveSubmarine (ShittySubmarine x y) (Down i) = ShittySubmarine x (y - i)
    position (ShittySubmarine x y) = (x, y)


data AdequateSubmarine = AdequateSubmarine Int Int Int deriving(Show)
instance Submarine AdequateSubmarine where
    moveSubmarine (AdequateSubmarine aim x y) (Forward i) = AdequateSubmarine aim (x + i) (y + aim * i)
    moveSubmarine (AdequateSubmarine aim x y) (Up i) = AdequateSubmarine (aim + i) x y
    moveSubmarine (AdequateSubmarine aim x y) (Down i) = AdequateSubmarine (aim - i) x y
    position (AdequateSubmarine aim x y) = (x, y)


result :: Submarine s => s -> Int
result = op . position
    where op (x, y) = x * (-y)


main :: IO ()
main = do
    actions <- parseLines <$!> readFile "data/day2.txt"

    let shittySub = foldl moveSubmarine (ShittySubmarine 0 0) <$> actions
    putStr "Shitty submarine: "
    print $ result <$> shittySub

    let betterSub = foldl moveSubmarine (AdequateSubmarine 0 0 0) <$> actions
    putStr "Better submarine: "
    print $ result <$> betterSub
