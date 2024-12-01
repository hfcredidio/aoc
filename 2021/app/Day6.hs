module Day6 where


import Control.Monad
import Data.Char
import Data.Dequeue


newtype Fish = Fish {age :: Int} deriving(Show)
type AgeCount = BankersDequeue Int -- Mapping from age to count
data FishCounts = FishCounts {newFish :: AgeCount, oldFish :: AgeCount} deriving(Show)


population :: FishCounts -> Int
population p = sum (newFish p) + sum (oldFish p)


incrementAt :: Int -> [Int] -> [Int]
incrementAt idx xs = a ++ (b + 1):c
    where (a, b:c) = splitAt idx xs


mkAgeCount :: [Fish] -> AgeCount
mkAgeCount = fromList . foldr (incrementAt . age) (replicate 7 0)

mkFishCounts :: [Fish] -> FishCounts
mkFishCounts fs = FishCounts (fromList $ replicate 9 0) (mkAgeCount fs)


step :: FishCounts -> Maybe FishCounts
step fc = do
    (ohead, obody) <- popFront $ oldFish fc
    (nhead, nbody) <- popFront $ newFish fc
    let newTail = ohead + nhead
    let newOldFish = pushBack obody newTail
    let newNewFish = pushBack nbody newTail
    return $ FishCounts newNewFish newOldFish 


parseFishes :: String -> [Fish]
parseFishes "" = []
parseFishes s = Fish (read h):parseFishes t
    where (h, t) = span isDigit $ dropWhile (not . isDigit) s

main :: IO ()
main = do
    fishes <- mkFishCounts . parseFishes . (!!0) . lines <$!> readFile "data/day6.txt"
    print $ show $ fmap population $ (!!80) $ iterate (step =<<) $ Just fishes
    print $ show $ fmap population $ (!!256) $ iterate (step =<<) $ Just fishes
