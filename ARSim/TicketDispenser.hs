{-# LANGUAGE RankNTypes #-}
module Main where

import NewARSim
import Test.QuickCheck
import Test.QuickCheck.Property
import Data.List(nub)
import System.Random





-- Simple server returning unique tickets to clients.
ticketDispenser :: AR c (ProvidedOperation () Int ())
ticketDispenser = component $
                     -- Create a port for remote operation.
                  do requestTicketP <- providedOperation
                     -- Variable holding number of issued tickets.
                     cur <- interRunnableVariable (0 :: Int)
                     excl <- exclusiveArea
                     probeRead "Read" cur
                     probeWrite "Write" cur
                     -- Code of the remote operation: return the ticket number and update state.
                     let rtBody = do --rteEnter excl
                                     Ok v <- rteIrvRead cur
                                     rteIrvWrite cur (v+1)
                                     --rteExit excl
                                     return v
                     -- serverRunnableN "Server" Concurrent [requestTicketP] (\() -> rtBody)
                     serverRunnable Concurrent [requestTicketP] (\() -> rtBody)
                     -- Export the port of the operation.
                     return (seal requestTicketP)

client :: Int -> AR c (RequiredOperation () Int ())
client n        = component $
                     -- Create a port for calling a remote operation and an output port for
                     -- reporting obtained tickets.
                  do requestTicketR <- requiredOperation
                     -- In a loop: Obtain a ticket and send its value to the output port.
                     let clientLoop = do Ok v <- rteCall requestTicketR ()
                                         printlog "Ticket" v
                                         clientLoop
                     -- runnableN ("Client" ++ show n) Concurrent [Init] clientLoop
                     runnable Concurrent [Init] clientLoop
                     return (seal requestTicketR)

-- Create a server instance and 4 client instances and connect them
test :: AR c ()
test            = do srv <- ticketDispenser
                     r1 <- client 1
                     r2 <- client 2
                     r3 <- client 3 
                     r4 <- client 4
                     connect r1 srv
                     connect r2 srv
                     connect r3 srv
                     connect r4 srv



mainTrivial = quickCheck prop_nodupes_triv

-- A property that passes, uses the trivial scheduler.
prop_nodupes_triv :: Small Int -> Bool
prop_nodupes_triv (Small n) = tickets == nub tickets where
  tickets = map measureValue $ probe trace "Ticket" :: [Int]
  trace = limitTrans ((abs $ n) +1) $ execSim TrivialSched test



main = quickCheck prop_nodupes

type RandomSeed     = Int
type SimulationTime = Small Int
prop_nodupes :: RandomSeed -> SimulationTime -> Property
prop_nodupes g (Small n) = report $ tickets == nub tickets where
  tickets = run_nodupes g n
  report  = counterexample ("\n"++showTrace g n) -- Reports a trace 

-- Use this to get the actual tickets for a given counterexample
run_nodupes :: RandomSeed -> Int -> [Int]
run_nodupes g n = map measureValue $ probe trace "Ticket"
  where
    trace = limitTrans ((abs $ n) +1) $ execSim (RandomSched (mkStdGen g)) test

showTrace :: RandomSeed -> Int -> String
showTrace g n = unlines $ map showMeasure (probes trace ["Read","Write","Ticket"] :: [Measure Int]) 
  where
    trace = limitTrans ((abs $ n) +1) $ execSim (RandomSched (mkStdGen g)) test

showMeasure :: Show a => Measure a -> String
showMeasure m = measureID m ++ ": " ++ show (measureValue m)



{-


printTrace' g n = putStrLn $ showTrace g n

printTrace :: Int -> Int -> IO ()
printTrace g n = mapM_ (putStrLn . showMeasure) (probes trace ["Read","Write"] :: [Measure Int]) where
  trace = limitTrans ((abs $ n) +1) $ execSim (RandomSched (mkStdGen g)) test


-- Hilarious that this instance is not in the library
-- instance Arbitrary StdGen where
--  arbitrary = mkStdGen `fmap` arbitrary -- I'm sure Michal has objections to this definition


-- Extract the tickets reported by the clients from the trace.
collectTickets :: ((forall c. AR c [Tag]) -> (Trace,[Tag])) -> Int -> [Int]
collectTickets sim k = [v |VInt v <- sendsTo ps t] where
  s@(Sim (t,ps)) = cutSim k $ Sim $ sim test

collectTickets' :: ((forall c. AR c [Tag]) -> (Trace,[Tag])) -> Int -> IO [Int]
collectTickets' sim k = do
  print s
  return [v |VInt v <- sendsTo ps t] where
  s@(Sim (t,ps)) = cutSim k $ Sim $ sim test

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

-}
