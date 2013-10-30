{-# LANGUAGE RankNTypes #-}
module Main where

import ARSim
import System.Random
import Test.QuickCheck

import Control.Monad.State.Lazy as S
import Control.Monad.Error

import Data.List(nub, sortBy)
import Data.Function(on)
import Data.Tree

import System.IO.Unsafe
import Data.IORef

main = main' Nothing >> return ()

main' repl = do 
  ior <- newIORef Nothing
  r <- quickCheckWithResult stdArgs {replay = repl } $ prop_norace ior
  Just x <- readIORef ior
  return (x,(usedSeed r, usedSize r))
  


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



-- Checks that there are no duplicate tickets being issued.
prop_norace :: IORef (Maybe Sim) -> Property
prop_norace ior = traceProp test $ \s -> if prop_norace' s 
     then True
     else unsafePerformIO (writeIORef ior (Just s) >> return False)
 
prop_norace' :: Sim -> Bool
prop_norace' s@(Sim (t,ps)) = let ticks = sendsTo ps t in ticks == nub ticks
