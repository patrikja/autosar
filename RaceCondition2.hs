{-# LANGUAGE RankNTypes #-}
module Main where

import ARSim
import System.Random

r1 pqe          = do sequence_ $ replicate 10 $ rte_send pqe (1::Int)

-- producer adds numbers to the queue.
producer        = do pqe <- providedQueueElement
                     runnable Concurrent [Init] (r1 pqe)
                     return (seal pqe)

r2 rqe irv      = do Ok x <- rte_receive rqe
                     Ok v <- rte_irvRead irv
                     rte_irvWrite irv (v+x)
                     return ()

-- consumer runs its runnable each time an element is added to the queue.
-- The runnable takes an element from the queue and adds its value to
-- its accumulator irv. It has two kinds of race conditions:
-- * The queue should be empty, but sending messages to it
--     does not trigger r2 when r2 is already Pending.
-- * Different instances of r2 clash when they concurrently update
--     the irv.
consumer        = component $
                  do irv <- interRunnableVariable (0 :: Int)
                     rqe <- requiredQueueElement 10
                     runnable Concurrent [ReceiveQ rqe] (r2 rqe irv)
                     return (seal rqe)

-- Fixed version of consumer, which eliminates the second race condition.
consumerEx      = component $
                  do ex <- exclusiveArea
                     irv <- interRunnableVariable (0 :: Int)
                     rqe <- requiredQueueElement 10
                     let r2 = do Ok x <- rte_receive rqe
                                 rte_enter ex
                                 Ok v <- rte_irvRead irv
                                 rte_irvWrite irv (v+x)
                                 rte_exit ex
                                 return ()
                     runnable Concurrent [ReceiveQ rqe] r2
                     return (seal rqe)

-- Version of consumer that fixes both race conditions.
consumerFix     = component $
                  do ex <- exclusiveArea
                     irv <- interRunnableVariable (0 :: Int)
                     rqe <- requiredQueueElement 10
                     let r2 = do
                            cx <- rte_receive rqe
                            case cx of
                              Ok x -> do
                                rte_enter ex
                                Ok v <- rte_irvRead irv
                                rte_irvWrite irv (v+x)
                                rte_exit ex
                                r2
                              NO_DATA -> return ()
                     runnable Concurrent [ReceiveQ rqe] r2
                     return (seal rqe)

test :: AR c (RQ Int ()) -> AR c ()
test consumerx  = do pqe <- component producer
                     rqe <- consumerx
                     connect pqe rqe

-- Run the buggy version with the randomised scheduler
main            = mainRand consumer

mainRand :: (forall c. AR c (RQ Int ())) -> IO ()
mainRand consumerx  = do rng <- newStdGen
                         putStrLn $ "Using seed: " ++ show rng
                         putTrace $ fst $ simulationRand rng (test consumerx)

--
main1            = putTrace $ fst $ simulationHead (test consumer)
