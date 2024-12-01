module Day8 where


import Control.Monad
import qualified Data.Set as S


splitLine :: String -> [String]
splitLine "" = []
splitLine s = a:splitLine (dropWhile (==' ') b)
    where (a, b) = span (/=' ') $ dropWhile (==' ') s


data Display = Display {inputs :: [S.Set Char], outputs :: [S.Set Char]}

parseLines :: String -> Display
parseLines s = Display inputs outputs
    where (a, _:b) = span (/='|') s
          inputs = map S.fromList $ splitLine a
          outputs = map S.fromList $ splitLine b


count1478s :: [S.Set Char] -> Int
count1478s s = length [i | i <- s, length i `elem` [2, 3, 4, 7]]


find :: Eq k => [(k, v)] -> k -> Maybe v
find [] k = Nothing
find ((m, v):ms) k = if m == k then Just v else find ms k


type Code = [(S.Set Char, Int)]


decode :: [S.Set Char] -> Code
decode inp = [
        (one, 1), (two, 2), (three, 3),
        (four, 4), (five, 5), (six, 6),
        (seven, 7), (eight, 8), (nine, 9), (zero, 0)
     ]
    where one   = head $ [i | i <- inp, length i == 2]
          seven = head $ [i | i <- inp, length i == 3]
          four  = head $ [i | i <- inp, length i == 4]
          eight = head $ [i | i <- inp, length i == 7]
          three = head $ [i | i <- inp, length i == 5, i `S.union` one == i]
          two   = head $ [i | i <- inp, length i == 5, i `S.union` four == eight]
          five  = head $ [i | i <- inp, length i == 5, i `notElem` [two, three]]
          nine  = head $ [i | i <- inp, length i == 6, five `S.union` one == i]
          six   = head $ [i | i <- inp, length i == 6, i `S.union` seven == eight]
          zero  = head $ [i | i <- inp, length i == 6, i `notElem` [six, nine]]


applyCode :: [S.Set Char] -> Code -> Maybe Int
applyCode ns code = foldl (\x y -> 10*x + y) 0 <$> numbers
    where numbers = mapM (find code) ns


main :: IO ()
main = do
    patterns <- map parseLines . lines <$!> readFile "data/day8.txt"
    print $ sum $ map (count1478s . outputs) patterns

    let codes = map (decode . inputs) patterns
    let theOutputs = map outputs patterns
    print $ sum <$> zipWithM applyCode theOutputs codes 
