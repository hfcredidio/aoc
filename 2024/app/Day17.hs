{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BinaryLiterals #-}
module Day17 where

import Debug.Trace
import Control.Monad
import Data.Maybe (catMaybes)
import Data.Bits
import Data.List (findIndex, tails)

-- Implement the state monad myself
-- for shits and giggles
newtype State m s a = State { runState :: s -> m (s, a) }

instance Monad m => Functor (State m s) where
    fmap f st = State $ \s -> do
        (s', a) <- runState st s 
        return (s', f a)

instance Monad m => Applicative (State m s) where
    pure x = State $ pure . (,x)
    stf <*> sta = State $ \s -> do
        (s' , f) <- runState stf s
        (s'', a) <- runState sta s'
        return (s'', f a)

instance Monad m => Monad (State m s) where
    return = pure
    sta >>= f = State $ \s -> do
        (s', a) <- runState sta s
        runState (f a) s'

get :: Monad m => State m s s
get  = State $ \s -> pure (s, s)

set :: Monad m => s -> State m s ()
set newS = State . const $ pure (newS, ())
---

data Cpu = Cpu { cpuA    :: Int
               , cpuB    :: Int
               , cpuC    :: Int
               , cpuPC   :: Int
               , cpuProg :: [Int] } deriving(Show)

data OpCode = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV
data Register = RA | RB | RC | RPC

parseOpCode :: Int -> OpCode
parseOpCode 0 = ADV 
parseOpCode 1 = BXL 
parseOpCode 2 = BST 
parseOpCode 3 = JNZ 
parseOpCode 4 = BXC 
parseOpCode 5 = OUT 
parseOpCode 6 = BDV 
parseOpCode 7 = CDV
parseOpCode _ = error "Invalid OpCode"

getRegister :: Register -> State Maybe Cpu Int
getRegister reg = do
    cpu <- get
    return $ case reg of
         RA -> cpuA cpu
         RB -> cpuB cpu
         RC -> cpuC cpu
         RPC -> cpuPC cpu

mapRegister :: Register -> (Int -> Int) -> State Maybe Cpu ()
mapRegister reg f = do
    val <- getRegister reg
    cpu <- get
    set $ case reg of
        RA -> cpu { cpuA = f val }
        RB -> cpu { cpuB = f val }
        RC -> cpu { cpuC = f val }
        RPC -> cpu { cpuPC = f val }

setRegister :: Register -> Int -> State Maybe Cpu ()
setRegister reg val = mapRegister reg (const val)

halt :: State Maybe Cpu a
halt = State $ const Nothing

advancePC :: State Maybe Cpu ()
advancePC = mapRegister RPC (+1)

readProg :: State Maybe Cpu Int
readProg = do
    Cpu { cpuPC = pc, cpuProg = prog } <- get
    advancePC
    if pc < length prog
        then return (prog !! pc)
        else halt

readOpCode :: State Maybe Cpu OpCode
readOpCode = parseOpCode <$> readProg

readOperand :: State Maybe Cpu Int
readOperand = readProg

readComboOperand :: State Maybe Cpu Int
readComboOperand = do
    cpu <- get
    let toCombo 4 = cpuA cpu
        toCombo 5 = cpuB cpu
        toCombo 6 = cpuC cpu
        toCombo 7 = error "Reserved combo"
        toCombo x | x <= 3 = x
        toCombo _ = error "Invalid Combo"
    toCombo <$> readOperand

evalOpCode :: OpCode -> State Maybe Cpu (Maybe Int)
evalOpCode ADV = do
    op <- readComboOperand
    mapRegister RA (`shiftR` op)
    return Nothing

evalOpCode BXL = do
    op <- readOperand
    mapRegister RB (`xor` op)
    return Nothing

evalOpCode BST = do
    op <- readComboOperand
    setRegister RB (op .&. 0b111)
    return Nothing

evalOpCode JNZ = do
    a <- getRegister RA
    if a == 0 
        then return () 
        else do
            op <- readOperand
            setRegister RPC op
    return Nothing

evalOpCode BXC = do
    _ <- readOperand
    c <- getRegister RC
    mapRegister RB (`xor` c)
    return Nothing

evalOpCode OUT = do
    op <- readComboOperand
    return . Just $ op .&. 0b111

evalOpCode BDV = do
    op <- readComboOperand
    a <- getRegister RA
    setRegister RB (a `shiftR` op)
    return Nothing

evalOpCode CDV = do
    op <- readComboOperand
    a <- getRegister RA
    setRegister RC (a `shiftR` op)
    return Nothing

runUntilHalts :: State Maybe s a -> s -> (s, [a])
runUntilHalts st s = case runState st s of
    Nothing      -> (s, [])
    Just (s', a) -> let (s'', as) = runUntilHalts st s'
                     in (s'', a:as)

fromRA :: Cpu -> Int -> [Int]
fromRA cpu rA =
    let cpu' = cpu { cpuA = rA }
        (_, output) = runUntilHalts (readOpCode >>= evalOpCode) cpu'
     in catMaybes output

findInput :: Cpu -> [Int] -> Int
findInput cpu outputs = head $ findAllInputs 0 outs
    where

    outs = tail $ reverse $ tails outputs

    findAllInputs :: Int -> [[Int]] -> [Int]
    findAllInputs accRA [out] = step accRA out
    findAllInputs accRA (out:outs) = concat [ findAllInputs accRA' outs | accRA' <- step accRA out ]

    step :: Int -> [Int] -> [Int]
    step accRA outs = [ accRA'
                      | i <- [0..7]
                      , let accRA' = (accRA `shiftL` 3) .|. i
                      , fromRA cpu accRA' == outs ]

main :: IO ()
main = do
    let prog = [0,1,5,4,3,0]
        cpu = Cpu { cpuA = 729, cpuB = 0, cpuC = 0, cpuPC = 0, cpuProg = prog }
        (_, output) = runUntilHalts (readOpCode >>= evalOpCode) cpu
    print $ catMaybes output

    let prog = [2,4,1,1,7,5,1,5,0,3,4,3,5,5,3,0]
        cpu = Cpu { cpuA = 56256477, cpuB = 0, cpuC = 0, cpuPC = 0, cpuProg =  prog}
        (_, output) = runUntilHalts (readOpCode >>= evalOpCode) cpu
    print $ catMaybes output

    print $ findInput cpu (cpuProg cpu)
