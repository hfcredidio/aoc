module Day15 where

import Control.Monad
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S

-- Parsing
parseDirection :: Char -> Direction
parseDirection '<' = West
parseDirection '>' = East
parseDirection '^' = North
parseDirection 'v' = South
parseDirection  c  = error $ "Invalid direction " ++ show c

parseSite :: Position -> Char -> Room Position -> Room Position
parseSite pos '@' (Room r b w) = Room pos b w
parseSite pos '.' room = room
parseSite pos '#' (Room r b w) = Room r b (pos:w)
parseSite pos 'O' (Room r b w) = Room r (pos:b) w
parseSite _ _ _ = error "Invalid"

parseRoom :: [String] -> Room Position
parseRoom lines = addSites emptyRoom
    where emptyRoom = Room (Position (0, 0)) [] []
          addSites = foldl1 (.) [ parseSite (Position (r, c)) site
                                | (r, line) <- zip [0..] lines
                                , (c, site) <- zip [0..] line ]
---

data Direction = North | South | East | West deriving(Show)

class Eq a => Moveable a where
    move :: a -> Direction -> a
    collides :: a -> a -> Bool

newtype Position = Position (Int, Int) deriving(Show, Eq, Ord)
instance Moveable Position where
    move (Position (r, c)) North = Position (r - 1, c)
    move (Position (r, c)) South = Position (r + 1, c)
    move (Position (r, c)) West  = Position (r, c - 1)
    move (Position (r, c)) East  = Position (r, c + 1)
    collides = (==)
    
data WidePosition = WidePosition Int Int Int deriving(Show, Eq)
instance Moveable WidePosition where
    move (WidePosition r c w) North = WidePosition (r - 1) c w
    move (WidePosition r c w) South = WidePosition (r + 1) c w
    move (WidePosition r c w)  East = WidePosition r (c + 1) w
    move (WidePosition r c w)  West = WidePosition r (c - 1) w
    WidePosition r c w `collides` WidePosition r' c' w'
        | r /= r' = False
        | c' <= c = (c - c') < w'
        | c  <= c'= (c' - c) < w
        | otherwise = error "A"

popCollision :: Moveable a => a -> [a] -> Maybe (a, [a])
popCollision x ys = case break (collides x) ys of
    (_, [])  -> Nothing
    (a, b:c) -> Just (b, a ++ c)

data Room a = Room { rRobot :: a , rBoxes :: [a] , rWalls :: [a] } deriving(Show)

moveBox :: Moveable a => a -> Direction -> Room a -> Maybe (Room a)
moveBox pos dir room@(Room rob boxes walls) = do
    let newBox = move pos dir
    guard $ isNothing (popCollision newBox walls)
    case popCollision newBox boxes of
        Nothing -> Just $ room { rBoxes = newBox:boxes }
        Just (box', boxes') -> do
            room' <- moveBox box' dir $ room { rBoxes = boxes' }
            moveBox pos dir room'

moveRobot :: Moveable a => Room a -> Direction -> Room a
moveRobot room@(Room rob boxes walls) dir  = fromMaybe room $ do
    let newRob = move rob dir
    guard $ isNothing (popCollision newRob walls)
    case popCollision newRob boxes of
        Nothing -> Just $ room { rRobot = newRob }
        Just (box', boxes') -> do
            room' <- moveBox box' dir $ room { rBoxes = boxes' }
            Just $ room' { rRobot = newRob }

widenRoom :: Room Position -> Room WidePosition
widenRoom (Room rob b w) = Room rob' b' w'
    where widen width (Position (r, c)) = WidePosition r (2*c) width
          rob' = widen 1 rob
          b' = map (widen 2) b
          w' = map (widen 2) w

main :: IO ()
main = do
    (rawRoom, _:rawMoves) <- break (=="") . lines <$!> readFile "data/Day15.txt"
    let moves = map parseDirection (concat rawMoves)
    let room = parseRoom rawRoom
    let final = foldl moveRobot room moves
    print $ sum [ 100 * r + c | Position (r, c) <- rBoxes final ]

    let room' = widenRoom room
    let final' = foldl moveRobot room' moves
    print $ sum [ 100 * r + c | WidePosition r c _ <- rBoxes final' ]
