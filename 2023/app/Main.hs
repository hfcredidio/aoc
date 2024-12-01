module Main where


import Control.Monad
import System.Environment
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
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
runDay ["01"] = putStrLn "\nDay01" >> Day01.main
runDay ["02"] = putStrLn "\nDay02" >> Day02.main
runDay ["03"] = putStrLn "\nDay03" >> Day03.main
runDay ["04"] = putStrLn "\nDay04" >> Day04.main
runDay ["05"] = putStrLn "\nDay05" >> Day05.main
runDay ["06"] = putStrLn "\nDay06" >> Day06.main
runDay ["07"] = putStrLn "\nDay07" >> Day07.main
runDay ["08"] = putStrLn "\nDay08" >> Day08.main
runDay ["09"] = putStrLn "\nDay09" >> Day09.main
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
runDay _      = putStrLn "I've no idea what you're talking about."


main :: IO ()
main = getArgs >>= runDay
