module Day7 where


import Control.Monad
import Data.Char


bisect :: (Eq a, Fractional a) => (a -> a) -> (a, a) -> Maybe (a, a)
bisect f (a, b) = case map signum [fa, fm, fb] of
    [ _,  0,  _] -> Just (m, m)
    [-1, -1,  1] -> Just (m, b)
    [ 1,  1, -1] -> Just (m, b)
    [ 1, -1, -1] -> Just (a, m)
    [-1,  1,  1] -> Just (a, m)
    _            -> Nothing
    where m = (a + b) / 2
          fa = f a
          fb = f b
          fm = f m


findRoot :: (Eq a, Fractional a) => (a -> a) -> (a, a) -> Maybe a
findRoot f p0 = fst <$> res !! 100
    where res = iterate (bisect f =<<) (Just p0)


differentiate :: (Eq a, Fractional a) => (a -> a) -> a -> a -> a
differentiate f dx x = (f (x + dx) - f x) / dx


linearCost :: Double -> Double
linearCost = id


squareCost :: Double -> Double
squareCost x = (x * (x + 1)) / 2


costFunction :: (Double -> Double) -> [Integer] -> Double -> Double
costFunction cost positions goal = sum $ map loss fPositions
    where fPositions = map fromInteger positions
          loss pos = cost $ abs (goal - pos)


getNumbers :: String -> [Integer]
getNumbers "" = []
getNumbers s = read h:getNumbers t
    where (h, t) = span isDigit $ dropWhile (not . isDigit) s


main :: IO ()
main = do
    positions <- getNumbers . head . lines <$!> readFile "data/day7.txt"

    let cost1 = costFunction linearCost positions
    let firstGoal = findRoot (differentiate cost1 0.001) (0, 500)
    print $ round . cost1 . fromInteger . round <$> firstGoal

    let cost2 = costFunction squareCost positions
    let secondGoal = findRoot (differentiate cost2 0.001) (0, 500)
    print $ round . cost2 . fromInteger . round <$> secondGoal
