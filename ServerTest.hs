{-# LANGUAGE RankNTypes #-}
module Main where

import ARSim
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Monad.State.Lazy as S
import Control.Monad.Error

import Data.List(nub)


main = quickCheck prop_norace

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
                     let -- r2 2 = return ()
                         r2 i  = do Ok v <- rte_call rop ()
                                    rte_send pqe v
                                    r2 (i+1)
                     runnable Concurrent [Init] (r2 0)
                     return (seal2 (rop, pqe))

test            = do t <- ticketDispenser
                     (rop1, pqe1) <- client
                     (rop2, pqe2) <- client
                     (rop3, pqe3) <- client
                     (rop4, pqe4) <- client
                     connect rop1 t
                     connect rop2 t
                     connect rop3 t
                     connect rop4 t
                     return [pqe1, pqe2, pqe3, pqe4]




-- Takes a simulation returning a list of queue-ports and returns all values
--  sent to those ports.
tickets :: Int -> (forall c. AR c [PQ a c1]) -> Schedule -> [Value]
tickets limit code sch =
          let (t, pqs) = simulationSched sch code
              labs     = sendsTo [a | PQ a <- pqs] (fmap (take limit) t)
          in [x|SND _ (x) _ <- labs]

-- Checks that there 
prop_norace :: Property
prop_norace = sized $ \n -> do
  let code  = test
      limit = (1+n*10)
      shrnk :: Schedule -> [Schedule]
      shrnk = shrinkSchedule limit (\g -> fst (simulationLoggingRand g code))
  forAllShrink genSchedule shrnk $ \sched -> 
    let ts = tickets limit code sched in ts == nub ts


-- Just tests that the rerun scheduler works with unmodified traces.    
testRerun = do
  g <- return $ (read "1968150254 2092437132")
  let (t,a) = simulationLoggingRand g test
      ls    = t
  mapM_ print (traceLabels $ fst t)
  putStrLn ""
  let (x,_) = simulationRerun t test
  mapM_ print (traceLabels x)
  print $ traceLabels (fst t) == traceLabels x
    
