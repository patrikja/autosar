{-# LANGUAGE RankNTypes #-}
module Main where

import ARSim
import System.Random


ticketDispenser :: AR c (PO () Int ())
ticketDispenser = do pop <- providedOperation
                     irv <- interRunnableVariable (0 :: Int)
                     let r1 = do Ok v <- rte_irvRead irv
                                 rte_irvWrite irv (v+1)
                                 return v
                     serverRunnable Concurrent [pop] (\() -> r1)
                     return (seal pop)

client :: AR c1 (RO () Int (), PQ Int ())
client          = do rop <- requiredOperation
                     pqe <- providedQueueElement
                     let r2 20 = return ()
                         --r2 i  = do Ok v <- rte_call rop ()
                         r2 i  = do rte_callAsync rop ()
                                    Ok v <- rte_result rop
                                    rte_send pqe v
                                    r2 (i+1)
                     runnable Concurrent [Init] (r2 0)
                     return (seal2 (rop, pqe))

test            = do t <- ticketDispenser
                     (rop, pqe) <- client
                     connect t rop

{-

mainRand :: (forall c. AR c (RQ Int ())) -> IO ()
mainRand consumerx  = do rng <- newStdGen
                         putStrLn $ "Using seed: " ++ show rng
                         putTrace $ simulationRand rng (test consumerx)

--
main1            = putTrace $ simulationHead (test consumer)
-}
