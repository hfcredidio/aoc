module Day08 where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M

data Node = Node { nValue :: String 
                 , nLeft  :: String
                 , nRight :: String
                 } deriving(Show) 

type Graph = M.Map String Node

parseNode :: String -> Node
parseNode str = let letters = filter isAlphaNum str
                    value  = take 3 letters
                    lValue = take 3 $ drop 3 letters
                    rValue = take 3 $ drop 6 letters
                 in Node value lValue rValue

step :: Graph -> Node -> Direction -> Node
step g n L = g M.! nLeft n
step g n R = g M.! nRight n

steps :: Graph -> [Direction] -> Node -> [Node]
steps graph directions start = scanl (step graph) start (cycle directions)

data Direction = L | R deriving(Show)

parseDirection :: Char -> Direction
parseDirection 'L' = L
parseDirection 'R' = R
parseDirection  c  = error $ "Invalid direction " ++ show c

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day08.txt"
    let directions = parseDirection <$> head input
    let nodes = parseNode <$> drop 2 input
    let graph = M.fromList $ zip (nValue <$> nodes) nodes
    let start = graph M.! "AAA"
    print $ length $ takeWhile (\n -> nValue n /= "ZZZ") $ steps graph directions start

    let isStart node = last (nValue node) == 'A'
    let isEnd node = last (nValue node) == 'Z'

    let starts = filter isStart nodes
    let allSteps = steps graph directions <$> starts

    -- I can't be bothered writing a prime decomposition function
    -- but all cycles are n * 283 so we can use that to compute
    let cycleLengths steps = length $ takeWhile (not . isEnd) steps
    print $ (283*) $ product $ (`div` 283) . cycleLengths <$> allSteps
