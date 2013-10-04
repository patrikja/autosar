module Main where

import ARSim

r1 pqe          = do rte_send pqe (123::Int)

c1              = do pqe <- providedQueueElement
                     runnable Concurrent [Init] (r1 pqe)
                     return (seal pqe)

r2 rqe          = do Ok x <- rte_receive rqe; return (Ok ())

c2              = do rqe <- requiredQueueElement 10
                     runnable Concurrent [ReceiveQ rqe] (r2 rqe)
                     return (seal rqe)

test            = do pqe <- component c1
                     rqe <- component c2
                     connect pqe rqe
                             
main            = simulation 0 test
