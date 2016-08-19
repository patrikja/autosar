-- | Testing task assignment annotations.

module Main where

import Control.Monad
import Data.Maybe
import NewARSim 
import System.Random
import Test.QuickCheck

type IntPort p = DataElem Unqueued Int p
type P a       = (a, a)
type C         = P (IntPort Required)

-- | @comp1@ runs every time "task1" is activated, and is mapped as the first
-- runnable in that task.
comp1 :: AUTOSAR (IntPort Provided)
comp1 = atomic $ do
  a <- providedPort
  s <- interRunnableVariable 0
  runnableT ["task1" :-> 0] (MinInterval 0) [TimingEvent 0.2] $ do
    Ok x <- rteIrvRead s
    rteWrite a x
    printlog "comp1" $ "write " ++ show x
    rteIrvWrite s (x + 1)
  return $ seal a 

-- | A task assignment in which we allow @comp2@ to run every second activation
-- of "task1".
comp2 :: AUTOSAR (IntPort Provided)
comp2 = atomic $ do
  a <- providedPort
  s <- interRunnableVariable 0
  runnableT ["task1" :>> (1, 2)] (MinInterval 0) [TimingEvent 0.4] $ do
    Ok x <- rteIrvRead s
    rteWrite a x
    printlog "comp2" $ "write " ++ show x
    rteIrvWrite s ((x + 1) `mod` 3)
  return $ seal a 

-- | Since @comp3@ depends on @comp2@ we would have to schedule it to run every
-- second activation (and last) in "task1" if we were to map it to that task.
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
  declareTask "task2" (DataReceivedEvent c2)


exampleQC :: Property
exampleQC = traceProp softw $ \trs ->
  counterexample (traceTable trs) $ all (isNothing . transError) . snd $ trs

main :: IO ()
main = do 
  g <- newStdGen
  simulateStandalone 2.0 printAll (RandomSched g) softw
  return ()

main2 :: IO ()
main2 = forever main 
