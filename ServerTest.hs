{-# LANGUAGE RankNTypes #-}
module Main where

import ARSim
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen

import Data.List(nub)

ticketDispenser :: AR c (PO () Int ())
ticketDispenser = component $
                  do pop <- providedOperation
                     irv <- interRunnableVariable (0 :: Int)
                     let r1 = do Ok v <- rte_irvRead irv
                                 rte_irvWrite irv (v+1)
                                 return v
                     serverRunnable Concurrent [pop] (\() -> r1)
                     return (seal pop)

client :: AR c (RO () Int (), PQ Int ())
client          = component $
                  do rop <- requiredOperation
                     pqe <- providedQueueElement
                     let r2 4 = return ()
                         r2 i  = do Ok v <- rte_call rop ()
                                    rte_send pqe v
                                    r2 (i+1)
                         -- r2 i  = do rte_callAsync rop ()
                         --           Ok v <- rte_result rop
                         --           rte_send pqe v
                         --           r2 (i+1)
                     runnable Concurrent [Init] (r2 0)
                     return (seal2 (rop, pqe))

test            = do t <- ticketDispenser
                     (rop1, pqe1) <- client
                     (rop2, pqe2) <- client
                     connect rop1 t
                     connect rop2 t
                     return (pqe1, pqe2)

main = do rng <- newStdGen
          let (t, (PQ a, PQ b)) = simulationRand rng test
              labs = sendsTo [a,b] t
              tickets = [x|SND _ x _ <- labs]
          mapM_ print tickets

