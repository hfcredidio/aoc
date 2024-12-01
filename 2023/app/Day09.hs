module Day09 where

type Series = [Int]

upgrade :: Series -> Series
upgrade = scanl1 (+)

downgrade :: Series -> Series
downgrade s = zipWith (-) s (tail s)

allEqual :: Eq a => [a] -> Bool
allEqual s = and $ zipWith (==) (tail s) s

forecast :: Series -> Int
forecast [] = error "A"
forecast s@(x:xs) | allEqual s = x
                  | otherwise  = x + forecast (downgrade s)

main :: IO ()
main = do
    input <- lines <$> readFile "data/Day09.txt"
    let serieses = reverse . map read . words <$> input :: [[Int]]
    print $ sum $ map forecast serieses
    print $ sum $ map (forecast . reverse) serieses
