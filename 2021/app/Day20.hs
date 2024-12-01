module Day20 where

import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Matrix as M

data State = Lit | Unlit deriving(Eq)
instance Show State where
    show Lit = "#"
    show Unlit = "."

fromChar :: Char -> State
fromChar '#' = Lit
fromChar '.' = Unlit
fromChar _ = error "Invalid char"

type Code = V.Vector State
type Image = (M.Matrix State, State)

getState :: Image -> Int -> Int -> State
getState (m, outside) r c = fromMaybe outside $ M.safeGet r c m

toInt :: [State] -> Int
toInt = toInt' 0
    where toInt' n [] = n
          toInt' n (Lit:xs) = toInt' (n * 2 + 1) xs
          toInt' n (Unlit:xs) = toInt' (n * 2) xs

neighborhood :: Image -> Int -> Int -> [State]
neighborhood img r c = getState img <$> [r-1..r+1] <*> [c-1..c+1]

decodeSite :: Image -> Code -> Int -> Int -> State
decodeSite img code r c = code V.! (toInt $ neighborhood img r c)

upscaleImage :: Code -> Image -> Image
upscaleImage code img@(m, outside) = (newM, newOutside)
    where newM = M.fromLists [ [ decodeSite img code r c | c <- [0..M.ncols m + 1] ] | r <- [0..M.nrows m + 1] ]
          newOutside = case outside of
            Lit -> V.last code
            Unlit -> V.head code

parseLine :: String -> [State]
parseLine = map fromChar

parseFile :: [String] -> (Image, Code)
parseFile lns = ((img, Unlit), code)
    where code = V.fromList $ parseLine $ head lns
          img = M.fromLists $ map parseLine $ drop 2 lns

countLit :: Image -> Int
countLit (img, _) = length $ filter (==Lit) $ M.toList img

main :: IO ()
main = do
    (img, code) <- parseFile . lines <$!> readFile "data/day20.txt"
    let imgs = iterate (upscaleImage code) img
    print $ countLit $ imgs !! 2
    print $ countLit $ imgs !! 50
