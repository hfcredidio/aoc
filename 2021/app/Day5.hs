module Day5 where


import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Set as S



getNumbers :: String -> [Int]
getNumbers "" = []
getNumbers s = read h:getNumbers t
    where (h, t) = span isDigit $ dropWhile (not . isDigit) s


type Point = (Int, Int)
data Line = Line Point Point deriving(Show) 

parseLine :: String -> Either String Line
parseLine s = case getNumbers s of
    [x0, y0, x1, y1] -> Right $ Line (x1, y1) (x0, y0)
    _                -> Left $ "Malformed line: " ++ s


smartRange :: Int -> Int -> [Int]
smartRange a b = [a, a + signum (b - a)..b]


expandLine :: Line -> [Point]
expandLine (Line (x0, y0) (x1, y1)) = zip (smartRange x0 x1) (smartRange y0 y1)


notDiag :: Line -> Bool
notDiag (Line (x0, y0) (x1, y1)) = x0 == x1 || y0 == y1


lineIntersections :: Line -> Line -> S.Set Point
lineIntersections l1 l2 = S.intersection (S.fromList $ expandLine l1) (S.fromList $ expandLine l2)


unionsMap :: Ord b => (a -> S.Set b) -> [a] -> S.Set b
unionsMap f [] = S.empty
unionsMap f xs = foldl1 S.union $ map f xs


allIntersections :: [Line] -> S.Set Point
allIntersections [] = S.empty
allIntersections (l:ls) = S.union h t
    where h = unionsMap (lineIntersections l) ls
          t = allIntersections ls


main :: IO ()
main = do
    theLines <- (map parseLine . lines) <$!> readFile "data/day5.txt"
    print $ show $ S.size . allIntersections . filter notDiag <$> sequence theLines
    print $ show $ S.size . allIntersections <$> sequence theLines
