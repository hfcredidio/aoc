module Day14 where

import Control.Monad
import Data.Maybe (isJust, fromMaybe)

type Point = (Int, Int)
type Range = (Point, Point)
data Line = VLine { lineX :: Int, lineY0 :: Int, lineY1 :: Int }
          | HLine { lineY :: Int, lineX0 :: Int, lineX1 :: Int } deriving(Show, Eq)

instance Ord Line where
    compare l1 l2 = compare (minY l1) (minY l2)

data BST a = Leaf | Node (BST a) a (BST a)

bstMin :: (Eq a, Ord a) => BST a -> a
bstMin Leaf = undefined
bstMin (Node Leaf x _) = x
bstMin (Node left _ _) = bstMin left

bstInsert :: (Eq a, Ord a) => a -> BST a -> BST a
bstInsert a Leaf = Node Leaf a Leaf
bstInsert a (Node left x right) | a <= x    = Node (bstInsert a left) x right
                                | otherwise = Node left x (bstInsert a right)

bstFind :: (Eq a, Ord a) => (a -> Bool) -> BST a -> Maybe a
bstFind pred Leaf = Nothing
bstFind pred (Node left x right) = case bstFind pred left of
    Nothing -> if pred x then Just x else bstFind pred right 
    found -> found

bstFromList :: (Eq a, Ord a) => [a] -> BST a
bstFromList = foldr bstInsert Leaf

bstToList :: BST a -> [a]
bstToList Leaf = []
bstToList (Node left x right) = bstToList left ++ x:bstToList right


bstInsertPoint :: Point -> BST Line -> BST Line
bstInsertPoint (a, b) Leaf = Node Leaf (HLine b a a) Leaf
bstInsertPoint p@(a, b) (Node left l@(HLine y x0 x1) right)
  | b == y && a == x0 - 1 = Node left (HLine y a x1) right
  | b == y && a == x1 + 1 = Node left (HLine y x0 a) right
  | b <= y = Node (bstInsertPoint p left) l right
  | b >  y = Node left l (bstInsertPoint p right)
bstInsertPoint p@(a, b) (Node left l@(VLine x y0 y1) right)
  | b <= y0 = Node (bstInsertPoint p left) l right
  | b >  y0 = Node left l (bstInsertPoint p right)

-- parsing

pairWise :: [a] -> [(a, a)]
pairWise = zip <*> tail

everyOther :: [a] -> [a]
everyOther (a:b:xs) = a:everyOther xs
everyOther [] = []
everyOther [x] = [x]

parsePoint :: String -> Point
parsePoint s = case break (==',') s of
    (x, ',':y) -> (read x, read y)
    _          -> error $ "Malformed point " ++ s

parseRanges :: String -> [Range]
parseRanges s = pairWise pts
    where pts = parsePoint <$> everyOther (words s)

fromRange :: Range -> Line
fromRange ((x, y), (a, b))
  | x == a = VLine x (min y b) (max y b)
  | y == b = HLine y (min x a) (max x a)
  | otherwise = undefined

-- business

inRange :: Int -> (Int, Int) -> Bool
inRange x (a, b) = a <= x && x <= b

intersect :: Line -> Line -> Bool
intersect (HLine y x0 x1) (HLine b a0 a1) = y == b && (x0 `inRange` (a0, a1) || a0 `inRange` (x0, x1))
intersect (VLine x y0 y1) (VLine a b0 b1) = x == a && (y0 `inRange` (b0, b1) || b0 `inRange` (y0, y1))
intersect (VLine x y0 y1) (HLine y x0 x1) = x `inRange` (x0, x1) && y `inRange` (y0, y1)
intersect x y = intersect y x

minY :: Line -> Int
minY (HLine y _ _) = y
minY (VLine _ y _) = y

maxY :: Line -> Int
maxY (HLine y _ _) = y
maxY (VLine _ _ y) = y

findIntersect :: Point -> BST Line -> Maybe Int
findIntersect (x, y) ls = do
    let vline = VLine x y 10000
    foundLine <- bstFind (intersect vline) ls
    return $ minY foundLine - 1

dropGrain :: Point -> BST Line -> Maybe Point
dropGrain (x, yy) ls = do
    y   <- findIntersect (x, yy) ls
    y'  <- findIntersect (x - 1, y + 1) ls
    if y' > y 
       then dropGrain (x-1, y + 1) ls
       else do
            y'' <- findIntersect (x + 1, y + 1) ls
            if y'' > y
               then dropGrain (x+1, y + 1) ls
               else return (x, y)

dropGrain' :: Point -> BST Line -> Maybe (BST Line)
dropGrain' p ls = do
    dropped <- dropGrain p ls
    return $ bstInsertPoint dropped ls


draw :: (Int, Int) -> (Int, Int) -> [Line] -> [String]
draw (x0, x1) (y0, y1) ls = [[if any (VLine x y y `intersect`) ls then '#' else '.' | x <- [x0..x1]] | y <-[y0..y1]]

main :: IO ()
main = do
    ranges <- bstFromList . map fromRange . concatMap parseRanges . lines <$!> readFile "data/Day14.txt"
    let hist = iterate (>>=dropGrain' (500, 0)) (Just ranges)
    print $ (+(-1)) $ length $ takeWhile isJust hist

    let maxy = maximum $ maxY <$> bstToList ranges
    let bottom = HLine (maxy+2) (-10000) 10000
    let ranges' = bstInsert bottom ranges
    let hist = iterate (>>=dropGrain' (500, 0)) (Just ranges')
    let notDone = (/= 0) . minY . bstMin . fromMaybe undefined
    print $ (+(-1)) $ length $ takeWhile notDone hist
