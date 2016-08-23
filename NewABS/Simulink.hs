module Main where

import NewARSim
import NewABS2
import System.Random

main :: IO ()
main = simulateUsingExternal absSystem >> return ()

