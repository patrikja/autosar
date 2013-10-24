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
main            = putTrace (simulationHead test)
