module Main where

import AUTOSAR

-- | Set this flag to @False@ to disable all task assignments.
main :: IO ()
main = do
  rng <- newTFGen
  simulateUsingExternal True rng simulinkACCgood1
--   simulateUsingExternal True rng simulinkACCbad1
  return ()
