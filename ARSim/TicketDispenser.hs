{-# LANGUAGE RankNTypes #-}
module Main where

import NewARSim
import Test.QuickCheck
import Test.QuickCheck.Property
import Data.List(nub)
import System.Random
import System.CPUTime


-- Simple server returning unique tickets to clients.
ticketDispenser :: AR c (ProvidedOperation () Int ())
ticketDispenser = component $ do 
                     requestTicketP <- providedOperation
                     cur <- interRunnableVariable (0 :: Int)
                     excl <- exclusiveArea
                     probeRead "Read" cur
                     probeWrite "Write" cur
                     let rtBody = do --rteEnter excl
                                     Ok v <- rteIrvRead cur
                                     rteIrvWrite cur (v+1)
                                     --rteExit excl
                                     return v
                     serverRunnable Concurrent [requestTicketP] (\() -> rtBody)
                     return (seal requestTicketP)

client :: Int -> AR c (RequiredOperation () Int ())
client n        = component $ do 
                     requestTicketR <- requiredOperation
                     let clientLoop = do Ok v <- rteCall requestTicketR ()
                                         printlog "Ticket" v
                                         clientLoop
                     runnable Concurrent [Init] clientLoop
                     return (seal requestTicketR)

-- Create a server instance and 3 client instances and connect them
system :: AR c ()
system          = do srv <- ticketDispenser
                     r1 <- client 1
                     r2 <- client 2
                     r3 <- client 3 
                     connect r1 srv
                     connect r2 srv
                     connect r3 srv


type RandomSeed     = Int
type SimulationTime = Int

check :: (RandomSeed -> SimulationTime -> [Measure Int]) -> IO ()
check example = quickCheck qprop 
  where qprop g (Small n) = counterexample ("\n"++ report measurement) $ tickets == nub tickets
          where tickets = map measureValue $ ticketsOnly measurement
                measurement = example g n

report ms = unlines $ map showMeasure $ ms
  where showMeasure m = measureID m ++ ": " ++ show (measureValue m)

ticketsOnly = filter ((=="Ticket") . measureID) 

run :: (RandomSeed -> SimulationTime -> [Measure Int]) -> IO ()
run example = do c <- getCPUTime
                 let measurement = example (fromInteger c) 40
                 putStrLn (report (ticketsOnly measurement))


example0 sched g n = probes ["Read","Write","Ticket"] $ limitTrans (abs n + 1) $ execSim sched system

example_triv g n = example0 TrivialSched g n

example_rr g n = example0 RoundRobinSched g n

example_rand g n = example0 (RandomSched (mkStdGen g)) g n


demo1 = run example_triv

demo2 = run example_rr

demo3 = run example_rand

demo4 = check example_rand

demo5 = check example_rr

demo6 = check example_triv

-- [Uncomment rteEnter and rteExit in the TicketDispenser and reload]

demo7 = check example_rand

demo8 = check example_rr

demo9 = check example_triv


main = demo4