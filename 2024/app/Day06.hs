module Day06 where

import Control.Monad
import Data.Maybe (catMaybes, isJust)
import Data.List (nub)
import qualified Data.Matrix as M

data Direction = U | D | L | R deriving(Eq, Show)
data Site = SEmpty | SBlock | SVisited [Direction] deriving(Eq)
data Guard = Guard { gPos :: (Int, Int)
                   , gDir :: Direction
                   } deriving(Show, Eq)

instance Show Site where
    show SEmpty = "."
    show SBlock = "#"
    show (SVisited _) = "@"

parseSite :: Char -> Site
parseSite '.' = SEmpty
parseSite '^' = SEmpty
parseSite '<' = SEmpty
parseSite '>' = SEmpty
parseSite 'v' = SEmpty
parseSite '#' = SBlock
parseSite c = error $ "Invalid site " ++ show c

findGuard :: [String] -> Guard
findGuard s = Guard (r, c) (mkDir d)
    where nrows = length s
          ncols = length $ head s
          elems = zip ((,) <$> [1..nrows] <*> [1..ncols]) (concat s)
          (r, c, d) = head [ (r, c, d) | ((r, c), d) <- elems , d `elem` "^<>v" ]
          mkDir '^' = U
          mkDir 'v' = D
          mkDir '<' = L
          mkDir '>' = R
          mkDir _ = error "Invalid direction"

move :: (Int, Int) -> Direction -> (Int, Int)
move (r, c) U = (r - 1, c)
move (r, c) D = (r + 1, c)
move (r, c) L = (r, c - 1)
move (r, c) R = (r, c + 1)

turnCW :: Direction -> Direction 
turnCW U = R
turnCW R = D
turnCW D = L
turnCW L = U

visitSite :: Guard -> M.Matrix Site -> M.Matrix Site
visitSite (Guard (r, c) dir) m = case M.getElem r c m of
    SBlock -> error "Trying to visit a block"
    SEmpty -> M.setElem (SVisited [dir]) (r, c) m
    SVisited dirs -> if dir `elem` dirs
                        then m
                        else M.setElem (SVisited (dir:dirs)) (r, c) m

stepGuard :: M.Matrix Site -> Guard -> Maybe (M.Matrix Site, Guard, Bool)
stepGuard room guard@(Guard (r, c) dir) =
        let guard' = guard { gPos = move (r, c) dir }
            (r', c') = gPos guard'
            room' = visitSite guard' room
        in case M.safeGet r' c' room of
            Nothing -> Nothing
            Just SEmpty -> Just (room', guard', False)
            Just SBlock -> Just (room, guard { gDir = turnCW dir }, False)
            Just (SVisited dirs) -> Just (room', guard', dir `elem` dirs)

data Path = Path { pPath :: [Guard], pIsCycle :: Bool } deriving(Show, Eq)

walkGuard :: M.Matrix Site -> Guard -> Path
walkGuard room guard = case stepGuard room guard of
    Nothing -> Path [guard] False
    Just (room', guard', False) ->
        let Path pos isCycle = walkGuard room' guard'
         in Path (guard:pos) isCycle
    Just (room', guard', True) -> Path [guard] True
                                
count :: Eq a => (a -> Bool) -> [a] -> Int
count pred = length . filter pred

main :: IO ()
main = do
    room <- M.fromLists . map (map parseSite) . lines <$!> readFile "data/Day06.txt"
    guard <- findGuard . lines <$!> readFile "data/Day06.txt"
    let path = walkGuard room guard
    let unqPos = nub [ (r, c) | Guard (r, c) _ <- pPath path ]
    print $ length unqPos
    print $ count pIsCycle [ walkGuard room' guard
                           | (i, (r, c)) <- zip [0..] $ tail unqPos
                           , let room' = M.setElem SBlock (r, c) room
                           ]
