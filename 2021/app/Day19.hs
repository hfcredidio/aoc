module Day19 where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Char (isDigit)

type Position = (Int, Int, Int)
newtype Rotation = Rotation (Position -> Position)
type Transform = (Position, Rotation)

instance Show Rotation where
    show (Rotation rot) = "Rotation " ++ show (rot (1, 2, 3))

idTransform :: Transform
idTransform = ((0, 0, 0), Rotation id)


rotations :: [Rotation]
rotations = map Rotation [ \(a, b, c) -> (a, b, c)  -- 0
            , \(a, b, c) -> (a, c, b)  -- 1
            , \(a, b, c) -> (b, a, c)  -- 2
            , \(a, b, c) -> (b, c, a)  -- 3
            , \(a, b, c) -> (c, a, b)  -- 4
            , \(a, b, c) -> (c, b, a)  -- 5
            , \(a, b, c) -> (-a, b, c)  -- 6
            , \(a, b, c) -> (-a, c, b)  -- 7
            , \(a, b, c) -> (-b, a, c)  -- 8
            , \(a, b, c) -> (-b, c, a)  -- 9
            , \(a, b, c) -> (-c, a, b)  -- 10
            , \(a, b, c) -> (-c, b, a)  -- 11
            , \(a, b, c) -> (a, -b, c)  -- 12
            , \(a, b, c) -> (a, -c, b)  -- 13
            , \(a, b, c) -> (b, -a, c)  -- 14
            , \(a, b, c) -> (b, -c, a)  -- 15
            , \(a, b, c) -> (c, -a, b)  -- 16
            , \(a, b, c) -> (c, -b, a)  -- 17
            , \(a, b, c) -> (a, b, -c)  -- 18
            , \(a, b, c) -> (a, c, -b)  -- 19
            , \(a, b, c) -> (b, a, -c)  -- 20
            , \(a, b, c) -> (b, c, -a)  -- 21
            , \(a, b, c) -> (c, a, -b)  -- 22
            , \(a, b, c) -> (c, b, -a)  -- 23
            , \(a, b, c) -> (-a, -b, c)  -- 24
            , \(a, b, c) -> (-a, -c, b)  -- 25
            , \(a, b, c) -> (-b, -a, c)  -- 26
            , \(a, b, c) -> (-b, -c, a)  -- 27
            , \(a, b, c) -> (-c, -a, b)  -- 28
            , \(a, b, c) -> (-c, -b, a)  -- 29
            , \(a, b, c) -> (-a, b, -c)  -- 30
            , \(a, b, c) -> (-a, c, -b)  -- 31
            , \(a, b, c) -> (-b, a, -c)  -- 32
            , \(a, b, c) -> (-b, c, -a)  -- 33
            , \(a, b, c) -> (-c, a, -b)  -- 34
            , \(a, b, c) -> (-c, b, -a)  -- 35
            , \(a, b, c) -> (a, -b, -c)  -- 36
            , \(a, b, c) -> (a, -c, -b)  -- 37
            , \(a, b, c) -> (b, -a, -c)  -- 38
            , \(a, b, c) -> (b, -c, -a)  -- 39
            , \(a, b, c) -> (c, -a, -b)  -- 40
            , \(a, b, c) -> (c, -b, -a)  -- 41
            , \(a, b, c) -> (-a, -b, -c)  -- 42
            , \(a, b, c) -> (-a, -c, -b)  -- 43
            , \(a, b, c) -> (-b, -a, -c)  -- 44
            , \(a, b, c) -> (-b, -c, -a)  -- 45
            , \(a, b, c) -> (-c, -a, -b)  -- 46
            , \(a, b, c) -> (-c, -b, -a)  -- 47
            ]


(.-) :: Position -> Position -> Position
(a, b, c) .- (d, e, f) = (a-d, b-e, c-f)

(.+) :: Position -> Position -> Position
(a, b, c) .+ (d, e, f) = (a+d, b+e, c+f)

intersection :: Ord a => [a] -> [a] -> [a]
intersection a b = S.toList $ S.intersection (S.fromList a) (S.fromList b)

findRelativePosition :: [Position] -> [Position] -> Maybe Position
findRelativePosition scanner1 rotatedScanner2 = case filter ((>=12) . snd) $ zip relativeDistances $ map matches relativeDistances of
    [] -> Nothing
    ((d,_):_) -> Just d
    where relativeDistances = (.-) <$> scanner1 <*> rotatedScanner2
          matches d = length $ intersection scanner1 $ map (.+d) rotatedScanner2

findTransform :: [Position] -> [Position] -> Maybe Transform
findTransform scanner1 scanner2 = case transforms of
    [] -> Nothing
    (t:_) -> Just t
    where allRotatedScanner2 = [map rot scanner2 | (Rotation rot) <- rotations]
          translations = map (findRelativePosition scanner1) allRotatedScanner2
          pair Nothing b = Nothing
          pair (Just a) b = Just (a, b)
          transforms = catMaybes $ zipWith pair translations rotations


andThen :: Transform -> Transform -> Transform
andThen t@(t1, Rotation rot1) (t2, Rotation rot2) = (transform t2 t, Rotation (rot1 . rot2))


findAllTransforms' :: [([Position], Transform)] -> [[Position]] -> [([Position], Transform)]
findAllTransforms' foundTransforms [] = foundTransforms
findAllTransforms' [] (s:scanners) = findAllTransforms' [(s, idTransform)] scanners
findAllTransforms' foundTransforms (s2:scanners) = case transforms of
    []     -> findAllTransforms' foundTransforms (scanners ++ [s2])
    (t:_)  -> findAllTransforms' (foundTransforms ++ [(s2, t)]) scanners
    where findTransform' (s1, t1) = case findTransform s1 s2 of
            Nothing -> Nothing
            Just t2 -> Just $ t1 `andThen` t2
          transforms = mapMaybe findTransform' foundTransforms


findAllTransforms :: [[Position]] -> [([Position], Transform)]
findAllTransforms = findAllTransforms' [] 

transform :: Position -> Transform -> Position
transform p (trans, Rotation rot) = rot p .+ trans

by3 :: [a] -> [(a, a, a)]
by3 (a:b:c:xs) = (a, b, c):by3 xs
by3 _ = []

getNumbers :: String -> [Int]
getNumbers [] = []
getNumbers ('-':c1:cs) | isDigit c1 = let (n:ns) = getNumbers (c1:cs)
                                       in -n:ns
getNumbers cs | isDigit $ head cs = let (ns, rest) = span isDigit cs
                                     in read ns:getNumbers rest
getNumbers (c:cs) = getNumbers cs


parseLines :: [String] -> [Position]
parseLines [] = []
parseLines (('-':'-':_):lns) = parseLines lns
parseLines (ln:lns) = case getNumbers ln of
    [a, b, c] -> (a, b, c):parseLines lns
    _ -> error "Malformed Line"


parseFile :: [String] -> [[Position]]
parseFile [] = []
parseFile content = parseLines lns:parseFile rest
    where (lns, rest) = span (/="") $ dropWhile (=="") content


manhattan :: Position -> Position -> Int
manhattan x y = sum $ map abs [a, b, c]
    where (a, b, c) = x .- y 

main :: IO ()
main = do
    a <- parseFile . lines <$!> readFile "data/day19.txt"
    let transforms = findAllTransforms a
    let allBeacons = [ map (`transform` transf) scannerPositions | (scannerPositions, transf) <- transforms ]
    let beacons = S.toList $ foldl1 S.union $ map S.fromList allBeacons
    print $ length beacons

    let scanners = map (fst . snd) transforms
    print $ maximum $ manhattan <$> scanners <*> scanners
