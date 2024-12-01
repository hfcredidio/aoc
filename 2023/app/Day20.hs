module Day20 where

import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.Map as M

data Toggle = On | Off deriving(Show, Eq)
data Level = High | Low deriving(Show, Eq)
data Pulse = Pulse { pSrc   :: String
                   , pLevel :: Level
                   , pDst   :: String
                   } deriving(Show, Eq)
data Module = Broadcaster
            | FlipFlop Toggle
            | Conjunction (M.Map String Level)
            | NoSuchModule
            deriving(Show, Eq)
data Network = Network { nChildren :: M.Map String [String]
                       , nModules  :: M.Map String Module
                       } deriving(Show, Eq)

emitPulse :: Pulse -> Module -> ([Level], Module)
emitPulse _ Broadcaster = ([Low], Broadcaster)
emitPulse _ NoSuchModule = ([], NoSuchModule)
emitPulse (Pulse _ High _) m@(FlipFlop _) = ([], m)
emitPulse (Pulse _ Low _) (FlipFlop Off) = ([High], FlipFlop On)
emitPulse (Pulse _ Low _) (FlipFlop On) = ([Low], FlipFlop Off)
emitPulse (Pulse src lev _) (Conjunction hist) =
    let hist' = M.insert src lev hist
     in if all (==High) (M.elems hist')
           then ([Low], Conjunction hist')
           else ([High], Conjunction hist')

emitPulses :: Pulse -> State Network [Pulse]
emitPulses p@(Pulse _ _ dst) = do
    (Network children modules) <- get
    let mod = M.findWithDefault NoSuchModule dst modules
        (level, mod') = emitPulse p mod
        emitted = Pulse dst <$> level <*> children M.! dst
        modules' = M.insert dst mod' modules
    put $ Network children modules'
    return emitted

catAccumulateST :: (b -> State a [b]) -> b -> State a [b]
catAccumulateST f b = catAccumulateST' f [b]
    where catAccumulateST' :: (b -> State a [b]) -> [b] -> State a [b]
          catAccumulateST' f [] = pure []
          catAccumulateST' f (x:xs) = do
              ys <- f x
              zs <- catAccumulateST' f (xs ++ ys)
              return (ys ++ zs)

iterateST :: State a b -> State a [b]
iterateST f = do
    x <- f
    xs <- iterateST f
    return (x:xs)

pressButton = let initPulse = (Pulse "" Low "broadcaster")
               in fmap (initPulse:) (catAccumulateST emitPulses initPulse)
pressButtonNTimes n = concat . take n <$> iterateST pressButton

parseChildren' :: String -> (String, [String])
parseChildren' ('%':ss) = parseChildren' ss
parseChildren' ('&':ss) = parseChildren' ss
parseChildren' ss = case words $ filter (/=',') ss of
    (src:"->":dsts) -> (src, dsts)
    _ -> error ""
parseChildren = M.fromList . map parseChildren'

parents :: M.Map String [String] -> String -> [String]
parents children child = [p | (p, cs) <- M.assocs children, child `elem` cs]

parseModule :: M.Map String [String] -> String -> (String, Module)
parseModule children ('b':_) = ("broadcaster", Broadcaster)
parseModule children ('%':s) = (head $ words s, FlipFlop Off)
parseModule children ('&':s) = (name, Conjunction ps)
    where name = head (words s)
          ps = M.fromList [(p, Low) | p <- parents children name]

parseModules :: M.Map String [String] -> [String] -> M.Map String Module
parseModules children = M.fromList . map (parseModule children)

parseNetwork :: [String] -> Network
parseNetwork ss = let children = parseChildren ss
                   in Network children (parseModules children ss)

part1 :: Network -> Int
part1 net = let ps = evalState (pressButtonNTimes 1000) net
                evalN h l [] = h * l
                evalN h l (Low:xs) = evalN h (l+1) xs
                evalN h l (High:xs) = evalN (h+1) l xs
             in evalN 0 0 $ map pLevel ps

part2 :: Network -> Int
part2 net = product $ map timeToHigh mods
    where buttonPresses = evalState (iterateST pressButton) net
          mods = concatMap (parents $ nChildren net) $ parents (nChildren net) "rx"
          timeToHigh name = head [ i
                                 | (i, p) <- zip [1..] buttonPresses
                                 , let p' = filter ((==name) . pSrc) p
                                 , any ((==High) . pLevel) p'
                                 ]


main :: IO ()
main = do
    input <- lines <$!> readFile "data/Day20.txt"
    let net = parseNetwork input
    print $ part1 net
    print $ part2 net
