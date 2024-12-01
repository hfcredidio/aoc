module Main where

import Control.Monad
import System.Environment

message = "Not done yet."

runDay :: [String] -> IO ()
runDay ["01"] = putStrLn message
runDay ["02"] = putStrLn message
runDay ["03"] = putStrLn message
runDay ["04"] = putStrLn message
runDay ["05"] = putStrLn message
runDay ["06"] = putStrLn message
runDay ["07"] = putStrLn message
runDay ["08"] = putStrLn message
runDay ["09"] = putStrLn message
runDay ["10"] = putStrLn message
runDay ["11"] = putStrLn message
runDay ["12"] = putStrLn message
runDay ["13"] = putStrLn message
runDay ["14"] = putStrLn message
runDay ["15"] = putStrLn message
runDay ["16"] = putStrLn message
runDay ["17"] = putStrLn message
runDay ["18"] = putStrLn message
runDay ["19"] = putStrLn message
runDay ["20"] = putStrLn message
runDay ["21"] = putStrLn message
runDay ["22"] = putStrLn message
runDay ["23"] = putStrLn message
runDay ["24"] = putStrLn message
runDay ["25"] = putStrLn message
runDay _      = putStrLn message


main :: IO ()
main = getArgs >>= runDay
