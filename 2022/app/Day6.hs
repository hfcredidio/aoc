module Day6 where

import Control.Monad
import Data.List
import Data.Functor
import Data.Function

nwise :: Int -> [a] -> [[a]]
nwise n xs = take n <$> iterate tail xs

countUnique :: Eq a => [a] -> Int
countUnique = length . nub

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

firstWhich :: (a -> Bool) -> [a] -> a
firstWhich pred xs = head $ filter pred xs

startOfMessage :: Int -> String -> Int
startOfMessage n s = nwise n s <&> countUnique & enumerate & firstWhich isValid & uncurry (+)
    where isValid (_, nuniq) = nuniq == n

main :: IO ()
main = do
    string <- readFile "data/Day6.txt"
    print $ startOfMessage 4 string
    print $ startOfMessage 14 string
