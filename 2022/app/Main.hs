module Main where


import Control.Monad
import System.Environment
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25


runDay :: [String] -> IO ()
runDay  ["1"] = putStrLn "\nDay1"  >> Day1.main
runDay  ["2"] = putStrLn "\nDay2"  >> Day2.main
runDay  ["3"] = putStrLn "\nDay3"  >> Day3.main
runDay  ["4"] = putStrLn "\nDay4"  >> Day4.main
runDay  ["5"] = putStrLn "\nDay5"  >> Day5.main
runDay  ["6"] = putStrLn "\nDay6"  >> Day6.main
runDay  ["7"] = putStrLn "\nDay7"  >> Day7.main
runDay  ["8"] = putStrLn "\nDay8"  >> Day8.main
runDay  ["9"] = putStrLn "\nDay9"  >> Day9.main
runDay ["10"] = putStrLn "\nDay10" >> Day10.main
runDay ["11"] = putStrLn "\nDay11" >> Day11.main
runDay ["12"] = putStrLn "\nDay12" >> Day12.main
runDay ["13"] = putStrLn "\nDay13" >> Day13.main
runDay ["14"] = putStrLn "\nDay14" >> Day14.main
runDay ["15"] = putStrLn "\nDay15" >> Day15.main
runDay ["16"] = putStrLn "\nDay16" >> Day16.main
runDay ["17"] = putStrLn "\nDay17" >> Day17.main
runDay ["18"] = putStrLn "\nDay18" >> Day18.main
runDay ["19"] = putStrLn "\nDay19" >> Day19.main
runDay ["20"] = putStrLn "\nDay20" >> Day20.main
runDay ["21"] = putStrLn "\nDay21" >> Day21.main
runDay ["22"] = putStrLn "\nDay22" >> Day22.main
runDay ["23"] = putStrLn "\nDay23" >> Day23.main
runDay ["24"] = putStrLn "\nDay24" >> Day24.main
runDay ["25"] = putStrLn "\nDay25" >> Day25.main
runDay []     = mapM_ (runDay . pure . show) [1..25]
runDay _      = putStrLn "I've no idea what you're talking about."


main :: IO ()
main = getArgs >>= runDay
