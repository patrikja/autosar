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

module Main where

import ARSim

-- | Just send 123 to the queue
r1 :: PQ Int c -> RunM (StdRet ())
r1 pqe          = do rte_send pqe (123::Int)

-- | Provides a port (create it) and run r1 every 1.0 time units
c1 :: AR c (PQ Int ())
c1              = do pqe <- providedQueueElement
                     runnable Concurrent [Timed 1.0] (r1 pqe)
                     return (seal pqe)

-- | Just receive a value and do nothing with it
r2 :: Valuable a => RQ a c -> RunM ()
r2 rqe          = do Ok x <- rte_receive rqe; return ()

-- | Requires a port (parametrised over the port) and "runs" r2
c2 :: AR c (RQ Int ())
c2              = do rqe <- requiredQueueElement 10 -- Queue of size 10
                     runnable Concurrent [ReceiveQ rqe] (r2 rqe)
                     return (seal rqe)

-- | Connect c1 and c2 to create a program that sends and receives.
test :: AR c ()
test            = do pqe <- component c1
                     rqe <- component c2
                     connect pqe rqe
                             
-- | Run the simulation of this test program, showing a trace of the execution
main            = putTraceLabels $ fst $ (simulationHead test)
