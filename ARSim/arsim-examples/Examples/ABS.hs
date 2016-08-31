module Main where

import AUTOSAR

-- | Set this flag to @False@ to disable all task assignments.
main :: IO ()
main = do
  simulateUsingExternal True simulinkABSgood1
--   simulateUsingExternal True simulinkABSbad1
  return ()
