module Day21 where

import qualified Data.Map as M

data OutcomeWeights outcome = OutcomeWeights (M.Map outcome Int)

type Score = Int
type Position = Int
type Population = Integer

data Player = Player1 | Player2 deriving(Eq, Ord, Show)

data PlayerState = PlayerState { pScore :: Score
                               , pPosition :: Position
                               } deriving(Eq, Ord, Show)

data GameState = GameState { p1State :: PlayerState
                           , p2State :: PlayerState
                           , turn :: Player
                           } deriving(Eq, Ord, Show)

type ParallelUniverses = M.Map GameState Population


player1Wins :: Score -> GameState -> Bool
player1Wins winScore (GameState ps1 _ _) = pScore ps1 >= winScore


player2Wins :: Score -> GameState -> Bool
player2Wins winScore (GameState _ ps2 _) = pScore ps2 >= winScore


count :: Ord a => [a] -> M.Map a Int
count xs = foldl (\m a -> M.insertWith (+) a 1 m) M.empty xs


quantumDie :: [OutcomeWeights Int]
quantumDie = repeat $ OutcomeWeights $ count $ (\i j k -> i + j + k) <$> [1..3] <*> [1..3] <*> [1..3]


deterministicDie :: [OutcomeWeights Int]
deterministicDie = deterministicDie' $ cycle [1..100]
    where deterministicDie' throws = OutcomeWeights (M.singleton (sum a) 1):deterministicDie' b
              where (a, b) = splitAt 3 throws
                                        

updateGameState :: GameState -> Int -> GameState
updateGameState (GameState ps1 ps2 Player1) deltaP = GameState newPlayer1State ps2 Player2
    where newPosition = ((pPosition ps1 + deltaP - 1) `mod` 10) + 1
          newScore = (pScore ps1) + newPosition
          newPlayer1State = PlayerState { pScore = newScore, pPosition = newPosition }
updateGameState (GameState ps1 ps2 Player2) deltaP = GameState ps1' ps2' Player1
    where (GameState ps2' ps1' _) = updateGameState (GameState ps2 ps1 Player1) deltaP


stepUniverses' :: ParallelUniverses -> ParallelUniverses -> OutcomeWeights Int -> ParallelUniverses
stepUniverses' partial pu outcome@(OutcomeWeights qa) = case M.minViewWithKey pu of
    Nothing -> partial
    Just ((gameState, count), popped) -> stepUniverses' x popped outcome
        where newPU = M.map ((*count) . fromIntegral) $ M.mapKeys (updateGameState gameState) qa
              x = M.unionWith (+) partial newPU
stepUniverses = stepUniverses' M.empty

popWins :: Score -> ParallelUniverses -> (ParallelUniverses, (Integer, Integer))
popWins winScore pu = (rest', (p1winCount, p2winCount))
    where (p1wins, rest)  = M.partitionWithKey (\k _ -> player1Wins winScore k) pu
          (p2wins, rest') = M.partitionWithKey (\k _ -> player2Wins winScore k) rest
          p1winCount = sum $ M.elems p1wins
          p2winCount = sum $ M.elems p2wins


type PUWinnerCount = (ParallelUniverses, (Integer, Integer))

stepWinnerCount :: Score -> PUWinnerCount -> OutcomeWeights Int -> PUWinnerCount
stepWinnerCount winScore (pu, (w1, w2)) die  = (pu', (w1+w1', w2+w2'))
    where (pu', (w1', w2')) = popWins winScore $ stepUniverses pu die

(.+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(a, b) .+ (c, d) = (a + c, b + d)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p xs = a ++ [b]
    where (a, b:_) = span p xs

loserScore :: GameState -> Score
loserScore (GameState p1 p2 Player1) = pScore p2
loserScore (GameState p1 p2 Player2) = pScore p1

main :: IO ()
main = do
    let init = GameState (PlayerState 0 1) (PlayerState 0 5) Player1

    let winnerCount = (M.singleton init 1, (0, 0))
    let gameHistory = takeWhile (not . M.null . fst) $ scanl (stepWinnerCount 1000) winnerCount deterministicDie
    let lastGameState = fst $ head $ M.toList $ fst $ last gameHistory
    print $ (length gameHistory) * 3 * (loserScore lastGameState)

    let winnerCount = (M.singleton init 1, (0, 0))
    let gameHistory = takeWhile' (not . M.null . fst) $ scanl (stepWinnerCount 21) winnerCount quantumDie
    print $ uncurry max $ snd $ last gameHistory
