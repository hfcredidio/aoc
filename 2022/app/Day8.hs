module Day8 where

import Data.List
import Control.Monad
import qualified Data.Map.Strict as M

isSeen :: [Int] -> [Bool]
isSeen xs = fst <$> tail res
    where isSeen' (_, curMax) x = (x > curMax, max curMax x)
          res = scanl isSeen' (False, -1) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt idx val lst = x ++ val:y
    where (x, _:y) = splitAt idx lst

scores' :: [Int] -> Int -> Int -> (Int, [Int])
scores' lastHeightIdx idx height = (idx - lastSeen, newLastHeightIdx)
    where lastSeen = maximum $ drop height lastHeightIdx
          newLastHeightIdx = insertAt height idx lastHeightIdx

scores :: [Int] -> [Int]
scores heights = fst <$> scores
    where initHeights = replicate 10 0
          computeScore (_, x) = uncurry $ scores' x
          scores = tail $ scanl computeScore (0, initHeights) $ zip [0..] heights

zipZipWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipZipWith f = zipWith (zipWith f)

(.&&) = zipZipWith (&&)
(.||) = zipZipWith (||)
(.*) = zipZipWith (*)

countTrues :: [[Bool]] -> Int
countTrues rows = sum $ length <$> map (filter id) rows

applyRows :: ([a] -> [b]) -> ([[a]] -> [[b]])
applyRows = map

applyCols :: ([a] -> [b]) -> ([[a]] -> [[b]])
applyCols f g = transpose $ f <$> transpose g

applyRevRows :: ([a] -> [b]) -> ([[a]] -> [[b]])
applyRevRows f g = reverse . f . reverse <$> g

applyRevCols :: ([a] -> [b]) -> ([[a]] -> [[b]])
applyRevCols f g = transpose $ reverse . f . reverse <$> transpose g

main :: IO ()
main = do
    lines <- lines <$!> readFile "data/Day8.txt"
    let forest = map (read . pure <$>) lines :: [[Int]]

    let n = applyCols    isSeen forest
        e = applyRevRows isSeen forest
        w = applyRows    isSeen forest
        s = applyRevCols isSeen forest
    print $ countTrues $ n .|| e .|| w .|| s

    let n = applyCols    scores forest
        e = applyRevRows scores forest
        w = applyRows    scores forest
        s = applyRevCols scores forest
    print $ maximum $ maximum <$> n .* e .* w .* s
