module Main where

import ARSim

r1 :: PQ Int c -> RunM (StdRet ())
r1 pqe          = do rte_send pqe (123::Int)

-- | Provides a port (create it)
c1 :: AR c (PQ Int ())
c1              = do pqe <- providedQueueElement
                     runnable Concurrent [Timed 1.0] (r1 pqe)
                     return (seal pqe)

r2 :: Valuable a => RQ a c -> RunM ()
r2 rqe          = do Ok x <- rte_receive rqe; return ()

-- | Requires a port (parametrised over the port)
c2 :: AR c (RQ Int ())
c2              = do rqe <- requiredQueueElement 10
                     runnable Concurrent [ReceiveQ rqe] (r2 rqe)
                     return (seal rqe)

-- | Connect the c1 and c2 
test :: AR c ()
test            = do pqe <- component c1
                     rqe <- component c2
                     connect pqe rqe
                             
main            = simulation 0 test
