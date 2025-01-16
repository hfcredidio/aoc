{-# LANGUAGE TypeSynonymInstances #-}

module Day19 where

import Debug.Trace
import Day17
import Data.List (isPrefixOf, nub)
import Control.Monad
import Control.Applicative
import qualified Data.Map as M

type Parser a = State [] String a

next :: Parser Char
next = do
    x <- get
    guard $ not (null x)
    set (tail x)
    return (head x)

nextN :: Int -> Parser String
nextN n = replicateM n next

obey :: (a -> Bool) -> Parser a -> Parser a
obey pred parser = do
    x <- parser
    guard $ pred x
    return x

parseChar :: Char -> Parser Char
parseChar c = obey (==c) next

parseString :: String -> Parser String
parseString s = obey (==s) $ nextN (length s)

anyOf :: [Parser a] -> Parser a
anyOf = foldl1 (<|>)

instance (Monad m, Alternative m) => Alternative (State m a) where
    p1 <|> p2 = State $ \s -> runState p1 s <|> runState p2 s
    empty = State $ const empty

endParse :: Show a => Parser a -> Parser a
endParse p = do
    x <- p
    s <- get
    guard $ null s
    return x

countParses :: Parser b -> String -> State [] (M.Map String Int) Int
countParses p "" = return 1
countParses p seq = do
    cache <- get
    case M.lookup seq cache of
        Just b -> return b
        Nothing -> do
            let (seqs', _) = unzip (runState p seq)
            counts <- mapM (countParses p) seqs'
            cache' <- get
            set $ M.insert seq (sum counts) cache'
            countParses p seq

main :: IO ()
main = do
    (patterns':_:designs) <- lines <$!> readFile "data/Day19.txt"
    let patterns = parseString <$> [ if last w == ',' then init w else w | w <- words patterns' ]

    let parsers = endParse $ some $ anyOf patterns
        result = map (runState parsers) designs
        countWhen pred = length . filter pred
    print $ countWhen (not . null) result

    let countParsesST = sum <$> mapM (countParses $ anyOf patterns) designs
    print $ snd . head $ runState countParsesST M.empty
