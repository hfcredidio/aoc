module Day05 where

import Control.Monad
import Data.Maybe (fromMaybe, catMaybes)
import Data.Foldable


type Range = (Int, Int)

rIndex :: Range -> Int -> Maybe Int
rIndex (a, b) val
  | val < a || b <= val = Nothing
  | otherwise = Just (val - a)

rValue :: Range -> Int -> Maybe Int
rValue (a, b) i
    | i < 0 || b-a <= i = Nothing
    | otherwise = Just (a + i)

mapValue :: (Range, Range) -> Int -> Maybe Int
mapValue (src, dst) val = do
    i <- rIndex src val
    rValue dst i

mapRange :: (Range, Range) -> Range -> (Maybe Range, Maybe Range, Maybe Range)
mapRange rm@((x, y), (x', y')) (a, b) = case map (mapValue rm) [a, b] of
    [Just a', Just b']          -> (Nothing    , Just (a', b'), Nothing)
    [Nothing, Just b']          -> (Just (a, x), Just (x', b'), Nothing)
    [Just a', Nothing]          -> (Nothing    , Just (a', y'), Just (y, b))
    [Nothing, Nothing] | b <  x -> (Just (a, b), Nothing      , Nothing)
    [Nothing, Nothing] | y <= a -> (Nothing    , Nothing      , Just (a, b))
    [Nothing, Nothing] | a <  x -> (Just (a, x), Just (x', y'), Just (y, b))

type RangeMap = [(Range, Range)]

mapMaybe f = catMaybes . map f

rmMapValue :: RangeMap -> Int -> Int
rmMapValue rms val = case mapMaybe (`mapValue` val) rms of
    (val':_)  -> val'
    []        -> val

rmMapRange :: RangeMap -> Range -> [Range] 
rmMapRange [] r = [r]
rmMapRange (rm:rms) r = a' ++ b' ++ c'
    where (a, b, c) = mapRange rm r
          a' = fromMaybe [] $ rmMapRange rms <$> a
          c' = fromMaybe [] $ rmMapRange rms <$> c
          b' = fromMaybe [] $ pure <$> b

parseMaps :: [String] -> [RangeMap]
parseMaps [] = []
parseMaps ("":lines) = parseMaps lines
parseMaps lines = (map mkMap ranges):maps
    where (_:ranges, rest) = span (/="") lines
          maps = parseMaps rest
          mkMap line = case read <$> words line of
              [a, b, c] -> ((b, b+c), (a, a+c))

toRanges :: [Int] -> [Range]
toRanges [] = []
toRanges (x:y:xs) = (x,x+y):toRanges xs

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day05.txt"
    let seeds = map read . tail . words $ head input :: [Int]
    let maps = parseMaps (tail input) :: [RangeMap]
    let mapSeed seed = foldl (flip rmMapValue) seed maps
    print $ minimum $ map mapSeed seeds

    let seedRanges = toRanges seeds
    let mapSeedRange rng = foldlM (flip rmMapRange) rng maps
    print $ minimum $ fst <$> concatMap mapSeedRange seedRanges
