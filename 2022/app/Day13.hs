module Day13 where

import Control.Monad
import Data.Char (isNumber)
import Data.List (sort, elemIndex)

type List = [Packet]
data Packet = PList List | PValue Int deriving(Show, Eq)

instance Ord Packet where
    compare (PList []) (PList []) = EQ
    compare (PList _ ) (PList []) = GT
    compare (PList []) (PList _ ) = LT
    compare x@(PValue _) y@(PList  _) = compare (PList [x]) y
    compare x@(PList  _) y@(PValue _) = compare x (PList [y])
    compare (PValue x) (PValue y) = compare x y
    compare (PList (x:xs)) (PList (y:ys)) = case compare x y of
      EQ -> compare (PList xs) (PList ys)
      c  -> c

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil pred [] = []
takeUntil pred (x:xs)
  | pred x    = [x]
  | otherwise = x:takeUntil pred xs

parsePacket :: [List] -> String -> Packet
parsePacket [p]  "" = PList p
parsePacket [p] "]" = PList p
parsePacket stack ('[':cs) = parsePacket ([]:stack) cs
parsePacket stack (']':cs) = case stack of
    (a:bs:stack') -> parsePacket ((bs ++ [PList a]):stack') cs
    _ -> undefined
parsePacket stack (' ':cs) = parsePacket stack cs
parsePacket stack (',':cs) = parsePacket stack cs
parsePacket (ps:stack) (c:cs) | isNumber c = parsePacket (s:stack) b
    where (a, b) = span isNumber (c:cs)
          p = PValue (read a)
          s = ps ++ [p]
parsePacket _ _ = undefined

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a:b:xs) = (a, b):pairs xs
pairs [x] = error "Odd sized list"

main :: IO ()
main = do
    packets <- map (parsePacket []) . filter (/="") . lines <$!> readFile "data/Day13.txt"
    let order = uncurry compare <$> pairs packets
    let correct = filter ((==LT) . fst) $ zip order [1..]
    print $ sum $ snd <$> correct
    let div1 = PList[PList[PValue 2]]
    let div2 = PList[PList[PValue 6]]
    let sorted = sort (div1:div2:packets)
    let result = do
            first <- elemIndex div1 sorted
            second <- elemIndex div2 sorted
            return $ (first + 1) * (second + 1)
    print result
