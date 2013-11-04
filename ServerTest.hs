{-# LANGUAGE RankNTypes #-}
module ServerTest where

import ARSim
import Test.QuickCheck
import Test.QuickCheck.Property
import Data.List(nub)

-- Simple server returning unique tickets to clients.
ticketDispenser :: AR c (PO () Int ())
ticketDispenser = component $
                  do pop <- providedOperation
                     irv <- interRunnableVariable (0 :: Int)
                     let r1 = do Ok v <- rte_irvRead irv
                                 rte_irvWrite irv (v+1)
                                 return v
                     serverRunnableN "Server" Concurrent [pop] (\() -> r1)
                     return (seal pop)

client :: Int -> AR c (RO () Int (), PQ Int ())
client n        = component $
                  do rop <- requiredOperation
                     pqe <- providedQueueElement
                     let -- r2 1 = return ()
                         r2  = do Ok v <- rte_call rop ()
                                  rte_send pqe v
                                  r2
                     runnableN ("Client" ++ show n) Concurrent [Init] r2
                     return (seal2 (rop, pqe))

test :: forall c. AR c [Tag]
test            = do t <- ticketDispenser
                     (rop1, pqe1) <- client 1
                     (rop2, pqe2) <- client 2
                     (rop3, pqe3) <- client 3 
                     (rop4, pqe4) <- client 4
                     connect rop1 t
                     connect rop2 t
                     connect rop3 t
                     connect rop4 t
                     return (tag [pqe1, pqe2, pqe3, pqe4])


collectTickets :: ((forall c. AR c [Tag]) -> (Trace,[Tag])) -> Int -> [Int]
collectTickets sim k = [v |VInt v <- sendsTo ps t] where
  (Sim (t,ps)) = cutSim k $ Sim $ sim test

sequential = simulationHead

random = simulationRand (read "1 1")

-- Checks that there are no duplicate tickets being issued.
prop_norace :: Property
prop_norace = tracePropS simulationHeadG test norace

-- Sim is the result of the simulation
norace :: Sim -> Bool
norace (Sim (t,ps)) = let ticks = sendsTo ps t in ticks == nub ticks

prop_norace_rand :: Property
prop_norace_rand = noShrinking $ traceProp test norace
 
prop_norace_rand_shrink :: Property
prop_norace_rand_shrink = traceProp test norace
 
main :: IO ()
main = quickCheck prop_norace

perfect :: IO ()
perfect = quickCheckWith stdArgs{replay = Just (read "1458293422 535353310",3)} prop_norace >> return ()

