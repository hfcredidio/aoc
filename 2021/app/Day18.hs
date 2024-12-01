module Day18 where

import Data.Functor
import Control.Monad
import Data.Char (isDigit)
import Data.Maybe (isJust, catMaybes, fromMaybe)


data BTree a = Pair (BTree a) (BTree a) | Value a
instance Show a => Show (BTree a) where
    show (Value v) = show v
    show (Pair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

data TreeContext a = ParentLeft (BTree a) | ParentRight (BTree a) deriving(Show)
type Zipper a = (BTree a, [TreeContext a])

{-zipper methods-}

treeRoot :: BTree a -> Zipper a
treeRoot tree = (tree, [])

zipUp :: Zipper a -> Maybe (Zipper a)
zipUp (_, [])               = Nothing
zipUp (n, ParentLeft  l:zs) = Just (Pair l n, zs)
zipUp (n, ParentRight r:zs) = Just (Pair n r, zs)

zipTop :: Zipper a -> Zipper a
zipTop z = case wayUp of
    [] -> z
    zs -> last zs
    where wayUp = catMaybes $ takeWhile isJust $ iterate (zipUp =<<) (Just z)

zipLeft :: Zipper a -> Maybe (Zipper a)
zipLeft (Value _, ctx) = Nothing
zipLeft (Pair l r, ctx) = Just (l, ParentRight r:ctx)

zipRight :: Zipper a -> Maybe (Zipper a)
zipRight (Value _, ctx) = Nothing
zipRight (Pair l r, ctx) = Just (r, ParentLeft l:ctx)

zipReplace :: Zipper a -> BTree a -> Zipper a
zipReplace (_, ctx) t = (t, ctx)

mkPair :: (a, a) -> BTree a
mkPair (a, b) = Pair (Value a) (Value b)

listValuesFromLeft' :: Zipper a -> Maybe [Zipper a]
listValuesFromLeft' z@(Value _, _) = Just [z]
listValuesFromLeft' z@(Pair l r, _) = do
    leftvalues <- zipLeft z >>= listValuesFromLeft'
    rightvalues <- zipRight z >>= listValuesFromLeft'
    return $ leftvalues ++ rightvalues

listValuesFromLeft = fromMaybe [] . listValuesFromLeft'

listValuesFromRight' :: Zipper a -> Maybe [Zipper a]
listValuesFromRight' z@(Value _, _) = Just [z]
listValuesFromRight' z@(Pair l r, _) = do
    leftvalues <- zipLeft z >>= listValuesFromRight'
    rightvalues <- zipRight z >>= listValuesFromRight'
    return $ rightvalues ++ leftvalues

listValuesFromRight = fromMaybe [] . listValuesFromRight'

leftValues :: Zipper a -> [Zipper a]
leftValues z@(_, ParentLeft _:_) = lvs ++ rest
    where up = Just z >>= zipUp
          lvs = fromMaybe [] $ up >>= zipLeft <&> listValuesFromRight
          rest = maybe [] leftValues up
leftValues z@(_, ParentRight _:_) = fromMaybe [] $ Just z >>= zipUp <&> leftValues
leftValues z@(_, []) = []

rightValues :: Zipper a -> [Zipper a]
rightValues z@(_, ParentRight _:_) = lvs ++ rest
    where up = Just z >>= zipUp
          lvs = fromMaybe [] $ up >>= zipRight <&> listValuesFromLeft
          rest = maybe [] rightValues up
rightValues z@(_, ParentLeft _:_) = fromMaybe [] $ Just z >>= zipUp <&> rightValues
rightValues z@(_, []) = []

{-parsing-}

parseValue :: String -> Maybe (BTree Int, String)
parseValue "" = Nothing
parseValue s@(c:cs) 
  | not $ isDigit c = Nothing
  | otherwise       = Just (Value $ read n, rest)
    where (n, rest) = span isDigit s

assert :: Bool -> Maybe ()
assert True  = Just ()
assert False = Nothing

parsePair :: String -> Maybe (BTree Int, String)
parsePair ('[':cs) = do
    (left, comma:rest) <- parseNumber cs
    assert $ comma == ','
    (right, rbrack:rest') <- parseNumber rest
    assert $ rbrack == ']'
    return (Pair left right, rest')
parsePair _ = Nothing

parseNumber :: String -> Maybe (BTree Int, String)
parseNumber "" = Nothing
parseNumber s@(c:_)
    | c == '['  = parsePair s
    | isDigit c = parseValue s
    | otherwise = Nothing

parseFile :: String -> [BTree Int]
parseFile = fromMaybe [] . mapM (fmap fst . parsePair) . lines 

{-task functions-}

explode :: Zipper Int -> Zipper Int
explode z@(Pair (Value lv) (Value rv), ctx) = y
    where replaced = (Value 0, ctx)
          x = case leftValues replaced of
              (Value l, ctx'):_ -> head $ rightValues (Value $ l + lv, ctx')
              _ -> replaced
          y = case rightValues x of
              (Value r, ctx''):_ -> (Value $ r + rv, ctx'')
              _ -> x
explode z = z

explodeAll :: Zipper Int -> Zipper Int
explodeAll z = expl
    where deep = filter (\(_, ctx) -> length ctx > 4) $ listValuesFromLeft z
          expl = case deep of []     -> zipTop z
                              (z':_) -> explodeAll $ zipTop $ maybe z' explode (zipUp z')

split :: Zipper Int -> Zipper Int
split z@(Value v, ctx)
    | v < 10    = z
    | otherwise = (Pair (Value l) (Value r), ctx)
        where l = floor (fromIntegral v / 2)
              r = ceiling (fromIntegral v / 2)
split z = z

splitted :: Zipper Int -> Maybe (Zipper Int)
splitted z = case toSplit of
    [] -> Nothing
    (z':_) -> Just $ zipTop $ split z'
    where shouldSplit (Value v) = v >= 10
          shouldSplit _ = False
          toSplit = filter (shouldSplit . fst) $ listValuesFromLeft z

reduce :: Zipper Int -> Zipper Int
reduce z = maybe expl reduce (splitted expl)
    where expl = explodeAll z

add :: BTree Int -> BTree Int -> BTree Int
add a b = res
    where (res, _) = reduce $ treeRoot $ Pair a b

          
magnitude :: BTree Int -> Int
magnitude (Value v) = v
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = [ (x, y)
              | (i, x) <- zip [0..] xs
              , (j, y) <- zip [0..] ys
              , i /= j ]

main :: IO ()
main = do
    numbers <- parseFile <$!> readFile "data/day18.txt"
    print $ magnitude $ foldl1 add numbers
    print $ maximum $ map (magnitude . uncurry add) (pairs numbers numbers)
