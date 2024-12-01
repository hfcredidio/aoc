module Day3 where

import qualified Data.Set as S
import Control.Monad
import Data.Char
import Data.List

data Item = Item Char deriving(Show, Eq, Ord)
type Compartment = S.Set Item
type Rucksack = (Compartment, Compartment)

mkItem :: Char -> Either String Item
mkItem c
    | isAlpha c = Right $ Item c
    | otherwise = Left $ "Invalid item " ++ show c

itemPriority :: Item -> Int
itemPriority (Item c)
    | isLower c = ord c - 96
    | isUpper c = itemPriority (Item $ toLower c) + 26
    | otherwise = undefined

parseRucksack :: String -> Either String Rucksack
parseRucksack s = do
    items <- sequence $ map mkItem s
    let count = length items
    let (c1, c2) = splitAt (count `div` 2) items
    return $ (S.fromList c1, S.fromList c2)

commonItem :: Rucksack -> Either String Item
commonItem (c1, c2) = case S.toList $ c1 `S.intersection` c2 of
    [itm] -> Right itm
    []    -> Left $ "No common item" ++ show c1 ++ " " ++ show c2
    _     -> Left $ "No common item" ++ show c1 ++ " " ++ show c2

commonPriority :: Rucksack -> Either String Int
commonPriority r = itemPriority <$> commonItem r

commonItemR :: [Rucksack] -> Either String Item
commonItemR rs = case S.toList intersec of
    [x] -> Right x
    x   -> Left $ show x
    where mergedComptartments = [S.union c1 c2 | (c1, c2) <- rs]
          intersec = foldl1 S.intersection mergedComptartments

split :: Int -> [a] -> [[a]]
split n [] = []
split n xs | length xs < n = [xs]
           | otherwise     = (take n xs):split n (drop n xs)

main :: IO ()
main = do
    lines <- lines <$!> readFile "data/Day3.txt"
    let rucks = sequence $ map parseRucksack lines
    let common = join $ sequence . map commonPriority <$> rucks
    print $ sum <$> common

    let groups = split 3 <$> rucks
    let commonGroups = join $ sequence <$> map commonItemR <$> groups
    let prio = map itemPriority <$> commonGroups
    print $ sum <$> prio
