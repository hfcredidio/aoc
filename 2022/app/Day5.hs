module Day5 where

import Control.Monad
import Data.List

type Crate = Char
type Pile = [Crate]
type Store = [Pile]

popPile :: Int -> Int -> Store -> (Pile, Store)
popPile count pileIdx store = (top, newStore)
    where (hd, pile:tl) = splitAt pileIdx store
          (top, popped) = splitAt count pile
          newStore = hd ++ popped:tl

insertPile :: Pile -> Int -> Store -> Store
insertPile addPile pileIdx store = hd ++ newPile:tl
    where (hd, pile:tl) = splitAt pileIdx store
          newPile = addPile ++ pile

mover9000 :: Int -> Int -> Int -> Store -> Store
mover9000 count from to store = insertPile (reverse pop) to s'
    where (pop, s') = popPile count from store

mover9001 :: Int -> Int -> Int -> Store -> Store
mover9001 count from to store = insertPile pop to s'
    where (pop, s') = popPile count from store

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs:chunks n (drop n xs)

parseChunk :: String -> Maybe Crate
parseChunk ('[':crate:']':_) = Just crate
parseChunk _                 = Nothing

insertInStore :: [Maybe Crate] -> Store -> Store
insertInStore crates store = [ insertInPile crate pile | (crate, pile) <- zip crates store]
    where insertInPile Nothing pile = pile
          insertInPile (Just c) pile = c:pile

parseStoreLine :: String -> Store -> Store
parseStoreLine line = insertInStore $ parseChunk <$> chunks 4 line

storeSize :: String -> Int
storeSize line = maximum $ read <$> words line

parseStore :: [String] -> Store
parseStore lines = build store
    where size = storeSize $ last lines
          store = take size $ repeat []
          build = foldr1 (.) (parseStoreLine <$> init lines)

parseMove :: String -> (Int, Int, Int)
parseMove s = case words s of
    ["move", count, "from", src, "to", dst] -> (read count, read src - 1, read dst - 1)
    _ -> undefined

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

main :: IO ()
main = do
    lines <- lines <$!> readFile "data/Day5.txt"
    let (rawStore, _:rawMoves) = break (=="") lines
    let store = parseStore rawStore
    let moves = parseMove <$> rawMoves
    let applyMoves mover moves store = foldr1 (.) (reverse actions) $ store
            where actions = uncurry3 mover <$> moves
    print $ map head $ applyMoves mover9000 moves store
    print $ map head $ applyMoves mover9001 moves store
