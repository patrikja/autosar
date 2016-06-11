{-
Copyright (c) 2014-2015, Johan Nordlander, Jonas Duregård, Michał Pałka,
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
module ServerTest where

import ARSim
import Test.QuickCheck
import Test.QuickCheck.Property
import Data.List(nub)

-- Simple server returning unique tickets to clients.
ticketDispenser :: AR c (PO () Int ())
ticketDispenser = component $
                     -- Create a port for remote operation.
                  do requestTicketP <- providedOperation
                     -- Variable holding number of issued tickets.
                     cur            <- interRunnableVariable (0 :: Int)
                     -- Code of the remote operation: return the ticket number and update state.
                     let rtBody = do Ok v <- rte_irvRead cur
                                     rte_irvWrite cur (v+1)
                                     return v
                     serverRunnableN "Server" Concurrent [requestTicketP] (\() -> rtBody)
                     -- Export the port of the operation.
                     return (seal requestTicketP)

client :: Int -> AR c (RO () Int (), PQ Int ())
client n        = component $
                     -- Create a port for calling a remote operation and an output port for
                     -- reporting obtained tickets.
                  do requestTicketR <- requiredOperation
                     outputPort <- providedQueueElement
                     -- In a loop: Obtain a ticket and send its value to the output port.
                     let clientLoop = do Ok v <- rte_call requestTicketR ()
                                         rte_send outputPort v
                                         clientLoop
                     runnableN ("Client" ++ show n) Concurrent [Init] clientLoop
                     -- Export both ports.
                     return (seal2 (requestTicketR, outputPort))

-- Create a server instance and 4 client instances, connect them and export the output ports of the clients
-- as the observable output.
test :: AR c [Tag]
test            = do srv <- ticketDispenser
                     (r1, out1) <- client 1
                     (r2, out2) <- client 2
                     (r3, out3) <- client 3 
                     (r4, out4) <- client 4
                     connect r1 srv
                     connect r2 srv
                     connect r3 srv
                     connect r4 srv
                     return (tag [out1, out2, out3, out4])








-- Extract the tickets reported by the clients from the trace.
collectTickets :: ((forall c. AR c [Tag]) -> (Trace,[Tag])) -> Int -> [Int]
collectTickets sim k = [v |VInt v <- sendsTo ps t] where
  s@(Sim (t,ps)) = cutSim k $ Sim $ sim test

collectTickets' :: ((forall c. AR c [Tag]) -> (Trace,[Tag])) -> Int -> IO [Int]
collectTickets' sim k = do
  print s
  return [v |VInt v <- sendsTo ps t] where
  s@(Sim (t,ps)) = cutSim k $ Sim $ sim test

sequential = simulationHead

random = simulationRand (read "1 1")

-- Checks that there are no duplicate tickets being issued.
prop_norace :: Property
prop_norace = tracePropS simulationHeadG test norace

-- Sim is the result of the simulation
norace :: Sim -> Bool
norace (Sim (t,ps)) = let ticks = sendsTo ps t in ticks == nub ticks

prop_norace_rand :: Property
prop_norace_rand = noShrinking $ traceProp test norace
 
prop_norace_rand_shrink :: Property
prop_norace_rand_shrink = traceProp test norace
 
-- quickCheck $ noShrinking  (traceProp test (\s@(Sim (t, ps)) -> counterexample (Data.Tree.drawForest $ (fmap (fmap show) $ toForest t)) (norace s)))

main :: IO ()
main = quickCheck prop_norace

perfect :: IO ()
perfect = quickCheckWith stdArgs{replay = Just (read "1458293422 535353310",3)} prop_norace >> return ()

