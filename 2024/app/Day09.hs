module Day09 where

import Control.Monad


everyOther :: [a] -> [a]
everyOther (x:y:xs) = x:everyOther xs
everyOther xs = xs

unzipL :: [a] -> ([a], [a])
unzipL xs = (everyOther xs, everyOther $ tail xs)

data Block = Block { bId    :: Int
                   , bStart :: Int
                   , bEnd   :: Int } deriving(Show)

bSize :: Block -> Int
bSize (Block _ s e) = e - s

data Free = Free { fStart :: Int
                 , fEnd   :: Int } deriving(Show)

fSize :: Free -> Int
fSize (Free s e) = e - s

compact :: Free -> Block -> (Block, Maybe Free, Maybe Block)
compact free@(Free fs fe) block@(Block bid bs be)
    | bs < fs = (block, Just free, Nothing)
    | bSize block == fSize free =
        let newBl = Block bid fs fe
         in (newBl, Nothing, Nothing)
    | bSize block < fSize free =
        let newBl = Block bid fs (fs + bSize block)
            remFree = Free (bEnd newBl) fe
         in (newBl, Just remFree, Nothing)
    | bSize block > fSize free =
        let newBl = Block bid fs fe
            remBl = Block bid bs (be - fSize free)
         in (newBl, Nothing, Just remBl)
    | otherwise = error "?"

compactMany :: [Free] -> [Block] -> [Block]
compactMany [] bls = bls
compactMany free [] = []
compactMany (f:fs) (b:bls) = 
    case  compact f b of
        (newBl, Just remFree, Just remBl) -> newBl:compactMany (remFree:fs) (remBl:bls)
        (newBl, Just remFree, Nothing)    -> newBl:compactMany (remFree:fs) bls
        (newBl, Nothing, Just remBl)      -> newBl:compactMany fs (remBl:bls)
        (newBl, Nothing, Nothing)         -> newBl:compactMany fs bls

checkSum :: Block -> Int
checkSum (Block bid bs be) = bid * sum [bs..be-1]

moveFile :: [Free] -> Block -> (Block, [Free])
moveFile [] bl = (bl, [])
moveFile (f:fs) bl@(Block bid bs be)
    | bStart bl < fStart f || fSize f < bSize bl =
        let (bl', fs') = moveFile fs bl
         in (bl', f:fs')
    | fSize f == bSize bl =
        let (Free start end) = f
            newBl = Block bid start end
         in (newBl, fs)
    | fSize f > bSize bl =
        let (Free start end) = f
            newBl = Block bid start (start + bSize bl)
            newFr = Free (bEnd newBl) end
         in (newBl, newFr:fs)
    | otherwise = error "?"

main :: IO ()
main = do
    nums <- map (read . pure) . init <$!> readFile "data/Day09.txt" :: IO [Int]
    let starts = scanl (+) 0 nums 
    let ends = tail starts
    let occupied = zipWith3 Block [0..] (everyOther starts) (everyOther ends)
    let free = zipWith Free (everyOther $ tail starts) (everyOther $ tail ends)
    let compacted = compactMany free (reverse occupied)
    print $ sum $ map checkSum compacted
    let compacted2 = scanl (\(_, free) block -> moveFile free block) (Block 0 0 0, free) (reverse occupied)
    print $ sum $ map (checkSum . fst) compacted2
