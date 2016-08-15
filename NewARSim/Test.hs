-- | Testing task assignment annotations.

module Main where

import Control.Monad
import NewARSim 
import System.Random

type IntPort p = DataElem Unqueued Int p
type P a       = (a, a)
type C         = P (IntPort Required)

comp1 :: AUTOSAR (IntPort Provided)
comp1 = atomic $ do
  a <- providedPort
  s <- interRunnableVariable 0
  runnableT ["task1" :-> 0] (MinInterval 0) [TimingEvent 0.2] $ do
    Ok x <- rteIrvRead s
    rteWrite a x
    rteIrvWrite s (x + 1)
  return $ seal a 

comp2 :: AUTOSAR (IntPort Provided)
comp2 = atomic $ do
  a <- providedPort
  s <- interRunnableVariable 0
  runnableT ["task1" :-> 1] (MinInterval 0) [TimingEvent 0.2] $ do
    Ok x <- rteIrvRead s
    rteWrite a x
    rteIrvWrite s ((x + 1) `mod` 31)
  return $ seal a 

comp3 :: AUTOSAR C
comp3 = atomic $ do
  c1 <- requiredPort
  c2 <- requiredPort
  runnableT ["task2" :-> 0] (MinInterval 0) [DataReceivedEvent c2] $ do
    Ok a <- rteRead c1
    Ok b <- rteRead c2
    printlog "comp3" (a, b) 
  return $ seal (c1, c2)

-- Triggering task2 on c2 works, but triggering on c1 fails since it has data
-- prior to c2, and comp3 is not pending.
softw :: AUTOSAR ()
softw = composition $ do
  a <- comp1
  b <- comp2
  (c1, c2) <- comp3
  connect a c1
  connect b c2
  declareTask "task1" (TimingEvent 0.2)
  declareTask "task2" (DataReceivedEvent c1)

main :: IO ()
main = do 
  g <- newStdGen
  simulateStandalone 2.0 printLogs (RandomSched g) softw
  return ()

main2 :: IO ()
main2 = forever main 
