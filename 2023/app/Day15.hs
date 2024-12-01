module Day15 where

import Debug.Trace
import Control.Monad
import Data.List
import Data.Char

data Lens = Lens { label :: String, focus :: Int } deriving(Show)
data Operation = Add Lens | Remove String deriving(Show)
type Boxes = [[Lens]]

parseOperation :: String -> Operation
parseOperation s = case span isAlpha s of
    (label, '=':focus) -> Add (Lens label (read focus))
    (label, "-") -> Remove label

hash :: String -> Int
hash = foldl step 0
    where step x y = ((x + fromEnum y) * 17) `mod` 256

hashLens :: Lens -> Int
hashLens = hash . label

wordsOn :: (a -> Bool) -> [a] -> [[a]]
wordsOn pred [] = []
wordsOn pred (x:xs) | pred x = wordsOn pred xs
wordsOn pred xs = a:wordsOn pred b
    where (a, b) = span (not . pred) xs

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst pred [] = []
removeFirst pred (x:xs)
  | pred x    = xs
  | otherwise = x:removeFirst pred xs

replaceFirst :: (a -> Bool) -> a -> [a] -> [a]
replaceFirst pred val [] = [val]
replaceFirst pred val (x:xs)
  | pred x    = val:xs
  | otherwise = x:replaceFirst pred val xs

mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt idx f []     = error "A"
mapAt 0   f (x:xs) = f x:xs
mapAt idx f (x:xs) = x:mapAt (idx-1) f xs

applyOperation :: Boxes -> Operation -> Boxes
applyOperation boxes (Remove lab) = mapAt h removeLens boxes
    where h = hash lab
          removeLens = removeFirst ((==lab) . label)

applyOperation boxes (Add lens) = mapAt h replaceLens boxes
    where h = hashLens lens
          lab = label lens
          replaceLens = replaceFirst ((== lab) . label) lens

focalPower :: Int -> [Lens] -> Int
focalPower boxId lenses = (boxId + 1) * (sum $ zipWith (*) [1..] focii)
    where focii = map focus lenses

totalFocalPower :: Boxes -> Int
totalFocalPower = sum . map (uncurry focalPower) . zip [0..]

main :: IO ()
main = do
    input <- wordsOn (==',') . head . lines <$!> readFile "data/Day15.txt"
    print $ sum $ hash <$> input

    let ops = parseOperation <$> input
    let boxes = foldl applyOperation (take 256 $ repeat []) ops
    print $ totalFocalPower boxes
