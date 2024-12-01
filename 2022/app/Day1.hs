module Day1 where

import Control.Monad
import Data.List

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen cond [] = []
splitWhen cond as = case t of
    []     -> [h]
    (y:ys) -> h:splitWhen cond ys
    where (h, t) = break cond as

(<$$>) :: (a -> b) -> [[a]] -> [[b]]
f <$$> aas = (f<$>) <$> aas

main :: IO ()
main = do
    lines <- lines <$!> readFile "data/Day1.txt"
    let elfCalories = sum <$> read <$$> splitWhen (=="") lines
    print $ maximum elfCalories
    print $ sum $ take 3 $ sortOn negate elfCalories
