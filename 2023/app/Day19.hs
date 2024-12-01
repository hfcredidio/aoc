module Day19 where

import Control.Monad
import Data.Char (isDigit)
import Data.List (lookup)
import Data.Maybe (fromJust)
import Data.Either (rights, lefts)

data Workflow = Workflow { wfLabel :: String
                         , wfCondition :: Condition
                         } deriving(Show)

data Compare = Lt | Gt deriving(Show)

data Condition = Accept
               | Reject
               | Goto String
               | IfElse { iePart  :: Char
                        , ieCmp   :: Compare
                        , ieValue :: Int
                        , ieTrue  :: Condition
                        , ieFalse :: Condition
                        } deriving(Show)

data Parts a = Parts { pX :: a
                     , pM :: a
                     , pA :: a
                     , pS :: a
                     } deriving(Show)


evalCmp :: Ord a => a -> Compare -> a -> Bool
evalCmp x Lt y = x < y
evalCmp x Gt y = x > y

groupFilter :: (a -> Bool) -> [a] -> [[a]]
groupFilter f [] = []
groupFilter f [x]
  | f x       = [[x]]
  | otherwise = [] 
groupFilter f (x:y:xs)
  | not (f x)          = groupFilter f (y:xs)
  | (f x) && not (f y) = [x]:groupFilter f xs
  | (f x) && (f y) = let (g:gs) = groupFilter f (y:xs)
                      in (x:g):gs

parseParts :: String -> Parts Int
parseParts s = case groupFilter isDigit s of
    [x, m, a, s] -> Parts (read x) (read m) (read a) (read s)
    _ -> error ""

parseCondition :: String -> Condition
parseCondition "A" = Accept
parseCondition "R" = Reject
parseCondition s = case s of
    (pt:cmp:rest) | cmp `elem` "><" ->
        IfElse { iePart  = pt
               , ieCmp   = cmpFromChar cmp
               , ieValue = read num
               , ieTrue  = parseCondition ifTrue
               , ieFalse = parseCondition ifFalse
               }
        where (cond, _:s') = span (/=':') s
              num = takeWhile isDigit rest
              (ifTrue, _:ifFalse) = span (/=',') s'
              cmpFromChar '>' = Gt
              cmpFromChar '<' = Lt
    s -> Goto s


parseWorkflow :: String -> Workflow
parseWorkflow s = Workflow label (parseCondition $ init cond)
    where (label, _:cond) = span (/='{') s

lookup' s = fromJust . lookup s

isAccepted :: Parts Int
           -> [(String, Workflow)]
           -> Condition
           -> Bool
isAccepted _ _ Accept = True
isAccepted _ _ Reject = False
isAccepted pts wfs (Goto s) = isAccepted pts wfs cond
    where cond = wfCondition (lookup' s wfs)
isAccepted pts wfs (IfElse pt cmp val ifTrue ifFalse ) =
    isAccepted pts wfs cond
    where cond = if evalCmp (getPart pt pts) cmp val
                    then ifTrue
                    else ifFalse

getPart :: Char -> Parts a -> a
getPart 'x' = pX
getPart 'm' = pM
getPart 'a' = pA
getPart 's' = pS
getPart  _  = error ""

setPart :: Char -> a -> Parts a -> Parts a
setPart 'x' val pts = pts { pX = val }
setPart 'm' val pts = pts { pM = val }
setPart 'a' val pts = pts { pA = val }
setPart 's' val pts = pts { pS = val }
setPart  _    _   _ = error ""
 
sumParts :: Parts Int -> Int
sumParts pt = sum (getPart <$> "xmas" <*> [pt])

type Range = (Int, Int)

splitRange :: Range -> Compare -> Int -> (Range, Range)
splitRange (a, b) Lt val
  | val < a    = ((0, 0), (a, b))
  | val < b    = ((a, val), (val, b))
  | otherwise  = ((a, b), (0, 0))
splitRange (a, b) Gt val
  | val < a    = ((a, b), (0, 0))
  | val < b    = ((val+1, b), (a, val+1))
  | otherwise  = ((0, 0), (a, b))

acceptedRanges :: Parts Range
               -> [(String, Workflow)]
               -> Condition
               -> [Either (Parts Range) (Parts Range)]
acceptedRanges pts _ Accept = [Right pts]
acceptedRanges pts _ Reject = [Left pts]
acceptedRanges pts wfs (Goto s) = acceptedRanges pts wfs cond
    where cond = wfCondition (lookup' s wfs)
acceptedRanges pts wfs (IfElse pt cmp val ifTrue ifFalse) =
    doT tr ++ doF fl
    where rng = getPart pt pts
          (tr, fl) = splitRange rng cmp val
          doT r = acceptedRanges (setPart pt r pts) wfs ifTrue
          doF r = acceptedRanges (setPart pt r pts) wfs ifFalse

countPossibilities :: Parts Range -> Int
countPossibilities (Parts x m a s) = product $ map size [x,m,a,s]
    where size (a, b) = b - a

main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day19.txt"
    let (top, _:bottom) = span (/="") input
    let workflows = [ (wfLabel wf, wf) | wf <- parseWorkflow <$> top ]
    let parts = parseParts <$> bottom
    let startCondition = wfCondition (lookup' "in" workflows)
    let isAcc pt = isAccepted pt workflows startCondition
    let accepted = filter isAcc parts
    print $ sum . map sumParts $ accepted

    let startRange = Parts (1, 4001) (1, 4001) (1, 4001) (1, 4001)
    let ranges = acceptedRanges startRange workflows startCondition 
    print $ sum $ map countPossibilities $ rights ranges
