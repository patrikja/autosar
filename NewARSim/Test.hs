-- | Testing task assignment annotations.

module Main where

import NewARSim 
import System.Random

type IntPort p = DataElem Unqueued Int p
type P a       = (a, a)
type C         = P (IntPort Required)

comp1 :: AUTOSAR (IntPort Provided)
comp1 = atomic $ do
  a <- providedPort
  s <- interRunnableVariable 0
  runnableT ["task1" :-> 0] (MinInterval 0) [TimingEvent 0.1] $ do
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

-- | This is probably a bad task-assignment:
-- 
-- Since 'comp3' is the only process assigned to @task2@ it will be scheduled
-- immediately. However, since 'comp3' relies on a @DataReceivedEvent@ to be
-- triggered, it is unlikely it will have status @Pending@ when this happens,
-- since both 'comp1' and 'comp2' will need to complete execution in @task1@
-- before this can happen.
comp3 :: AUTOSAR C
comp3 = atomic $ do
  c1 <- requiredPort
  c2 <- requiredPort
  runnableT ["task1" :-> 2, "task2" :-> 0] 
            -- (MinInterval 0) 
            Concurrent 
            [DataReceivedEvent c2] $ do
    Ok a <- rteRead c1
    Ok b <- rteRead c2
    printlog "comp3" (a, b) 
  return $ seal (c1, c2)

-- Declare tasks.
softw :: AUTOSAR ()
softw = composition $ do
  a <- comp1
  b <- comp2
  (c1, c2) <- comp3
  connect a c1
  connect b c2

  declareTask "task1" 0.1
  declareTask "task2" 0.1

main :: IO ()
main = do 
  g <- newStdGen
  simulateStandalone 5.0 printLogs (RandomSched g) softw
  putStrLn "--> done"
