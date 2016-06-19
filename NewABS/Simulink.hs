module Main where

import NewARSim
import NewABS
import System.Random

main :: IO ()
main = simulateUsingExternal abs_system

