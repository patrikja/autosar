module Main where

import ARSim

r1 pqe          = do sequence_ $ replicate 10 $ rte_send pqe (1::Int)

c1              = do pqe <- providedQueueElement
                     runnable Concurrent [Init] (r1 pqe)
                     return (seal pqe)

r2 rqe irv      = do Ok x <- rte_receive rqe
                     Ok v <- rte_irvRead irv
                     rte_irvWrite irv (v+x)
                     return ()

c2              = do irv <- interRunnableVariable (0 :: Int)
                     rqe <- requiredQueueElement 10
                     runnable Concurrent [ReceiveQ rqe] (r2 rqe irv)
                     return (seal rqe)

test            = do pqe <- component c1
                     rqe <- component c2
                     connect pqe rqe

test'           = do rqe <- component c2
                     pqe <- component c1
                     connect pqe rqe
                             
main            = simulation 0 test'
