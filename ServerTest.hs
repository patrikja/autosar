{-# LANGUAGE RankNTypes #-}
module Main where

import ARSim
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Monad.State.Lazy as S
import Control.Monad.Error

import Data.List(nub, sortBy)
import Data.Function(on)
import Data.Tree

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
                     let -- r2 1 = return ()
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
                     return (tag [pqe1, pqe2, pqe3, pqe4])



oneRun :: IO Sim 
oneRun = do 
  s <- newStdGen
  print s
  return $ Sim $ simulationRand s test
  
parentSort :: Sim -> Sim 
parentSort (Sim ((x,t),z)) = Sim ((x,t'),z) where
  t' :: [SchedulerOption]
  t' = sortBy (compare `on` (snd . fst . snd)) t
         
-- A simulation trace along with a list of ports to observe
newtype Sim = Sim (Trace, [Tag])
instance Show Sim where
  show (Sim (t,_)) = drawForest (map (fmap showNode) $ toForest t) where
   
-- Take a finite initial part of the simulation trace
cutSim :: Int -> Sim -> Sim
cutSim n (Sim (t,xs)) = Sim (fmap (take n) t, xs)

-- Shrink the simulation trace and rerun it
-- This would work much better if the trace was a tree, branching on new processes.
shrinkSim :: (forall c. AR c [Tag]) -> Sim -> [Sim]
shrinkSim code (Sim (trc,_)) = [rerun tn'| tn' <- shrinkTrace trc ] where
  rerun tns = Sim $ simulationRerun tns code

  
-- Checks that there are no duplicate tickets being issued.
prop_norace :: Property
(prop_norace,prop) = (\x -> (x,prop)) $ sized $ \n -> do
  let code  = test
      limit = (1+n*10)
      gen :: Gen Sim
      gen = fmap (cutSim limit . Sim) $ simulationRandG code
      shrnk :: Sim -> [Sim]
      shrnk = shrinkSim code
              -- shrinkNothing -- Disable shrinking
      
      prop :: Sim -> Bool
      prop (Sim (t,ps)) = let ticks = sendsTo ps t in ticks == nub ticks
  forAllShrink gen shrnk prop


-- Just tests that the rerun scheduler works with unmodified traces.    
testRerun = do
  g <- newStdGen
  let (t,a) = simulationRand g test
      ls    = t
  mapM_ print (take 100 $ traceLabels t)
  putStrLn ""
  let ((x,_)) = simulationRerun t test
  mapM_ print (take 100 $  traceLabels x)
  print $ take 100 (traceLabels t) == (take 100 $ traceLabels x)
    

{- -- Some code for debugging the shrinker     
rerun = let ((x,_)) = simulationRerun fakeTrace test in do
  mapM_ print (drop 22 $ traceLabels x)
  putStrLn ""
  mapM_ print (drop 22 $ unshrinkable)
  print $ length $ takeWhile id (zipWith (==) (traceLabels x) unshrinkable)
fakeTrace :: Trace
fakeTrace = ((undefined,zip unshrinkable (repeat undefined))) where
  ps = [(4,6),(12,14),(8,10),(16,18)]
fakeSim = Sim ((undefined,zip t (repeat undefined)),ps) where
  t  = unshrinkable
  ps = [(4,6),(12,14),(8,10),(16,18)]
unshrinkable = []
-}