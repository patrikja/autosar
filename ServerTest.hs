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
                     (rop1, pqe1@(PQ p1)) <- client
                     (rop2, pqe2@(PQ p2)) <- client
                     (rop3, pqe3@(PQ p3)) <- client
                     (rop4, pqe4@(PQ p4)) <- client
                     connect rop1 t
                     connect rop2 t
                     connect rop3 t
                     connect rop4 t
                     return [p1, p2, p3, p4]




         
-- A simulation trace along with a list of ports to observe
newtype Sim = Sim ((Trace, [Int]), [(Name,Name)])
instance Show Sim where
  show (Sim ((t,_),_)) = unlines (map show $ traceLabels t)

-- Taka a finite initial part of the simulation trace
cutSim :: Int -> Sim -> Sim
cutSim n (Sim ((t,ns),xs)) = Sim ((fmap (take n) t, take n ns), xs)

-- Shrink the simulation trace and rerun it
-- This would work much better if the trace was a tree, branching on new processes.
shrinkSim :: (forall c. AR c [(Name,Name)]) -> Sim -> [Sim]
shrinkSim code (Sim (trc, _)) = [rerun tn'| tn' <- shrnk trc ] where
  rerun tns = Sim $ simulationRerun tns code
  shrnk ((init,t),ns) = fmap (\t' -> ((init,t'),[])) $ shrinkList shrinkNothing t
  
-- Checks that there are no duplicate tickets being issued.
prop_norace :: Property
prop_norace = sized $ \n -> do
  let code  = test
      limit = (1+n*10)
      gen :: Gen Sim
      gen = fmap (cutSim limit . Sim) $ simulationLoggingRandG code
      shrnk :: Sim -> [Sim]
      shrnk = shrinkSim code
            -- shrinkNothing -- Disable shrinking
      
      prop :: Sim -> Bool
      prop (Sim ((t,_),ps)) = let ticks = [x |SND _ x _ <- sendsTo ps t] in ticks == nub ticks
  forAllShrink gen shrnk prop


-- Just tests that the rerun scheduler works with unmodified traces.    
testRerun = do
  g <- newStdGen
  let (t,a) = simulationLoggingRand g test
      ls    = t
  mapM_ print (take 100 $ traceLabels $ fst t)
  putStrLn ""
  let ((x,_),_) = simulationRerun t test
  mapM_ print (take 100 $  traceLabels x)
  print $ take 100 (traceLabels (fst t)) == (take 100 $ traceLabels x)
    
