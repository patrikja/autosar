module Main where

import AUTOSAR

-- | Set this flag to @False@ to disable all task assignments.
main :: IO ()
main = do
--   simulateUsingExternal True simulinkACCgood1
  simulateUsingExternal True simulinkACCbad1
  return ()
