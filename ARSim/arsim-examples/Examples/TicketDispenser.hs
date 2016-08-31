{-
Copyright (c) 2014-2016, Johan Nordlander, Jonas Duregård, Michał Pałka,
                         Patrik Jansson and Josef Svenningsson
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
   * Neither the name of the Chalmers University of Technology nor the names of its
     contributors may be used to endorse or promote products derived from this
     software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE RankNTypes #-}
module Main where

import AUTOSAR.ARSim
import Data.List(nub)
import System.Random
import System.CPUTime
import Test.QuickCheck
--import Test.QuickCheck.Property
import Debug.Trace (trace)

-- Simple server returning unique tickets to clients.
--ticketDispenser :: AR c (ProvidedOperation () Int ())
ticketDispenser = atomic $ do
                     requestTicketP <- providedPort
                     cur <- interRunnableVariable (0 :: Int)
                     excl <- exclusiveArea
                     probeRead "Read" cur
                     probeWrite "Write" cur
                     serverRunnable Concurrent [OperationInvokedEvent requestTicketP] $ \() -> do
                                --rteEnter excl
                                Ok v <- rteIrvRead cur
                                rteIrvWrite cur (v+1)
                                --rteExit excl
                                return v
                     return (seal requestTicketP)

--client :: Int -> AR c (RequiredOperation () Int ())
client n        = atomic $ do
                     requestTicketR <- requiredPort
                     let clientLoop = do 
                                Ok v <- rteCall requestTicketR ()
                                printlog "Ticket" v
                                clientLoop
                     runnable Concurrent [InitEvent] clientLoop
                     return (seal requestTicketR)

-- Create a server instance and 3 client instances and connect them
--system :: AR c ()
system :: AUTOSAR ()
system          = do srv <- ticketDispenser
                     r1 <- client 1
                     r2 <- client 2
                     r3 <- client 3
                     connect srv r1
                     connect srv r2
                     connect srv r3


type RandomSeed     = Int
type SimulationTime = Int

check :: (RandomSeed -> SimulationTime -> [Measure Int]) -> IO ()
check = undefined
--check example = quickCheck qprop
--  where qprop g (Small n) = counterexample ("\n"++ report measurement) $ tickets == nub tickets
--          where tickets = map measureValue $ ticketsOnly measurement
--                measurement = example g n

report ms = unlines $ map showMeasure $ ms
  where showMeasure m = measureID m ++ ": " ++ show (measureValue m)

ticketsOnly = filter ((=="Ticket") . measureID)

run :: (RandomSeed -> SimulationTime -> [Measure Int]) -> IO ()
run example = do c <- getCPUTime
                 let measurement = example (fromInteger c) 40
                 putStrLn (report (ticketsOnly measurement))


example0 :: Data a => SchedChoice -> t -> Int -> [Measure a]
example0 sched g n = probes ["Read","Write","Ticket"] $ limitTrans (abs n + 1) $ execSim False sched system

example_triv g n = example0 TrivialSched g n

example_rr g n = example0 RoundRobinSched g n

example_rand g n = example0 (RandomSched (mkStdGen g)) g n

exampleQCDebug :: Property
exampleQCDebug = traceProp system (\tt -> let t :: [Measure Int]; t = probes ["Ticket"] tt in let t' = [ snd $ measureTimeValue x | x <- t] in counterexample (show t) $ counterexample (show t') $ counterexample (unlines $ map show $ snd tt) $ counterexample' (traceTable tt) $ counterexample (unlines $ concat [ [show p, traceTable tt'] | (p, tt') <- shrinkTrace' system tt]) $ counterexample (unlines $ ["deleteLast:"] ++ (map traceTable $ shrinkTrace'' system tt))(t' == nub t') )

exampleQC :: Property
exampleQC = traceProp system (\tt -> let t :: [Measure Int]; t = probes ["Ticket"] tt in let t' = [ snd $ measureTimeValue x | x <- t] in counterexample (traceTable tt) $ t' == nub t' )


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


main = demo3
