module Day10 where

import Control.Monad

newtype Cpu = Cpu { cpuX :: Int } deriving(Show)
data Opcode = Noop | AddX { value :: Int } deriving(Show)

cycleCount :: Opcode -> Int
cycleCount Noop     = 1
cycleCount (AddX _) = 2

applyOpcode :: Opcode -> Cpu -> Cpu
applyOpcode Noop cpu = cpu
applyOpcode (AddX val) (Cpu x) = Cpu (x + val)

applyOpcodes :: [Opcode] -> Cpu -> [Cpu]
applyOpcodes [] cpu = []
applyOpcodes (op:ops) cpu = replicate cycles cpu ++ applyOpcodes ops upCpu
    where upCpu = applyOpcode op cpu
          cycles = cycleCount op

parseOpcode :: String -> Opcode
parseOpcode s = case words s of
    ["noop"]      -> Noop
    ["addx", num] -> AddX (read num)
    _             -> undefined

signalStrength :: [Cpu] -> Int
signalStrength cpus = sum $ zipWith (*) checks states
    where checks = [20, 60, 100, 140, 180, 220]
          states = [cpuX $ cpus !! (i - 1) | i <- checks]

drawPixel :: Cpu -> Int -> Char
drawPixel (Cpu x) cycle = if abs (x - pos) <= 1 then '#' else '.'
    where pos = cycle `mod` 40

chunksOf :: Int -> [a] -> [[a]]
chunksOf size [] = []
chunksOf size xs = x:chunksOf size y
    where (x, y) = splitAt size xs

main :: IO ()
main = do
    cmds <- map parseOpcode . lines <$!> readFile "data/Day10.txt"
    let cpuHistory = applyOpcodes cmds (Cpu 1)
    print $ signalStrength cpuHistory
    mapM_ print $ chunksOf 40 $ zipWith drawPixel cpuHistory [0..]
