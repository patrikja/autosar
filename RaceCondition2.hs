module Main where

import ARSim
import System.Random

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
                             
main            = putTrace $ simulationHead test'

main1           = do rng <- newStdGen
                     putStrLn $ "Using seed: " ++ show rng
                     putTrace $ simulationRand rng test'
                     
-- Interesting example. It has two kinds of race conditions:
-- * The queue should be empty, but sending messages to it
--     does not trigger r2 when r2 is already Pending.
-- * Different instances of r2 clash when they concurrently update
--     the irv.
example1 = putTrace $ simulationRand (read "651623791 2147483398") test'

