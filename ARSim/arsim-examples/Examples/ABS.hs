module Main where

import AUTOSAR

-- | Set this flag to @False@ to disable all task assignments.
main :: IO ()
main = do
  rng <- newTFGen
  simulateUsingExternal True rng simulinkABSgood1
--   simulateUsingExternal True rng simulinkABSbad1
  return ()
