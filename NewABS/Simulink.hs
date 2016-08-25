module Main where

import NewARSim
import NewABS2
import System.Random

main :: IO ()
main = do
  simulateUsingExternal True absSystem
  return ()

