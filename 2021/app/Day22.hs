module Day22 where

import Data.Foldable (foldlM)
import Control.Monad
import Data.Char (isDigit)

type Interval = (Int, Int)
type Point = (Int, Int, Int)
data Cube = Cube Interval Interval Interval deriving(Eq, Show)
data Action = On Cube | Off Cube deriving(Eq, Show)
data Axis = XAxis Int | YAxis Int | ZAxis Int deriving(Eq, Show)

{-parsing-}

getNumbers :: String -> [Int]
getNumbers [] = []
getNumbers ('-':c1:cs) | isDigit c1 = let (n:ns) = getNumbers (c1:cs)
                                       in -n:ns
getNumbers cs | isDigit $ head cs = let (ns, rest) = span isDigit cs
                                     in read ns:getNumbers rest
getNumbers (c:cs) = getNumbers cs

parseAction :: String -> Action
parseAction ('o':'n':xs) = On (parseCube xs)
parseAction ('o':'f':'f':xs) = Off (parseCube xs)
parseAction _ = error "Malformed line"

parseCube :: String -> Cube
parseCube s = case getNumbers s of
    [x1, x2, y1, y2, z1, z2] -> Cube (x1, x2+1) (y1, y2+1) (z1, z2+1)
    _ -> error "Malformed Line"


{-interval stuff-}

overlap :: Interval -> Interval -> Bool
overlap (a, b) (x, y) = not (y < a || b <= x)

notOverlap :: Interval -> Interval -> Bool
notOverlap i = not . overlap i

inRange :: Int -> Interval -> Bool
inRange x (a, b) = a <= x && x < b

findIntervalSplits :: Interval -> Interval -> [Int]
findIntervalSplits itvl (a, b) = concatMap (findSplit' itvl) [a, b]
    where findSplit' (x, y) a = if x <= a && a < y then [a] else []

emptyInterval :: Interval -> Bool
emptyInterval (x, y) = y <= x

{-cube stuff-}

isDisjoint :: Cube -> Cube -> Bool
isDisjoint (Cube x1 y1 z1) (Cube x2 y2 z2) = x1 `notOverlap` x2 || y1 `notOverlap` y2 || z1 `notOverlap` z2

isEmpty :: Cube -> Bool
isEmpty (Cube x y z) = any emptyInterval [x, y, z]

notEmpty :: Cube -> Bool
notEmpty = not . isEmpty

splitCube :: Cube -> Axis -> [Cube]
splitCube cube@(Cube (x1, x2) y z) (XAxis x) = if x `inRange` (x1, x2)
                                             then filter notEmpty [Cube (x1, x) y z,  Cube (x, x2) y z]
                                             else [cube]
splitCube cube@(Cube x (y1, y2) z) (YAxis y) = if y `inRange` (y1, y2) 
                                             then filter notEmpty [Cube x (y1, y) z,  Cube x (y, y2) z]
                                             else [cube]
splitCube cube@(Cube x y (z1, z2)) (ZAxis z) = if z `inRange` (z1, z2)
                                             then filter notEmpty [Cube x y (z1, z),  Cube x y (z, z2)]
                                             else [cube]

findCubeSplits :: Cube -> Cube -> [Axis]
findCubeSplits c1@(Cube x1 y1 z1) c2@(Cube x2 y2 z2) 
  | c1 `isDisjoint` c2 = []
  | otherwise = xsplit ++ ysplit ++ zsplit
    where xsplit = XAxis <$> findIntervalSplits x1 x2
          ysplit = YAxis <$> findIntervalSplits y1 y2
          zsplit = ZAxis <$> findIntervalSplits z1 z2

fullyEnclosed :: Cube -> Cube -> Bool
fullyEnclosed c1 c2 = all (inCube c2) (ends c1)
    where ends (Cube (x1, x2) (y1, y2) (z1, z2)) = [(x1,y1,z1),(x2-1,y2-1,z2-1)]
          inCube (Cube x y z) (a, b, c) = a `inRange` x && b `inRange` y && c `inRange` z

cubeDifference :: Cube -> Cube -> [Cube]
cubeDifference c1 c2
  | c1 `isDisjoint` c2 = [c1]
  | otherwise = filter (not . (`fullyEnclosed`c2)) splits
    where splits = foldM splitCube c1 $ findCubeSplits c1 c2

cubeUnion :: Cube -> Cube -> [Cube]
cubeUnion c1 c2 
  | c1 `isDisjoint` c2 = [c1, c2]
  | c1 `fullyEnclosed` c2 = [c2]
  | c2 `fullyEnclosed` c1 = [c1]
  | otherwise = c2:cubeDifference c1 c2

cubeIntersection :: Cube -> Cube -> [Cube]
cubeIntersection c1 c2 
  | c1 `isDisjoint` c2 = []
  | otherwise = filter (`fullyEnclosed`c2) splits
    where splits = foldM splitCube c1 $ findCubeSplits c1 c2

cubeVolume :: Cube -> Integer
cubeVolume (Cube x y z) = product $ map ilen [x, y, z]
    where ilen (a, b) = fromIntegral $ b - a

moveCube :: Cube -> Point -> Cube
moveCube (Cube x y z) (a, b, c) = Cube (move x a)  (move y b) (move z c)
    where move (a, b) x = (a + x, b + x)

applyAction :: [Cube] -> Action -> [Cube]
applyAction cs (On c) = c:concatMap (`cubeDifference`c) cs
applyAction cs (Off c) = concatMap (`cubeDifference`c) cs

main :: IO ()
main = do
    actions <- map parseAction . lines <$!> readFile "data/day22.txt"
    let finalCubes = foldl applyAction [] actions
    let centerCube = Cube (-50, 51)  (-50, 51) (-50, 51)
    let totalVolume = sum . map cubeVolume
    print $ totalVolume $ concatMap (cubeIntersection centerCube) finalCubes
    print $ totalVolume $ finalCubes
