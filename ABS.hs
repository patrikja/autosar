module ABS where
        
import ARSim
import System.Random

data SeqState =
        Stopped |
        Open Int Int |
        Closed Int Int

instance Valuable SeqState where
        toVal Stopped           = Void
        toVal (Open c l)        = VArray [VBool True, VInt c, VInt l]
        toVal (Closed c l)      = VArray [VBool False, VInt c, VInt l]
        fromVal Void            = Stopped
        fromVal (VArray [VBool True, VInt c, VInt l])
                                = Open c l
        fromVal (VArray [VBool False, VInt c, VInt l])
                                = Closed c l
        
relief_tick excl state valve wheelacc = do
        rte_enter excl
        Ok s <- rte_irvRead state
        case s of
                Stopped                            -> rte_irvWrite state s
                Open count limit   | count < limit -> rte_irvWrite state (Open (count+1) limit)
                                   | otherwise     -> relief_close state valve
                Closed count limit | count < limit -> rte_irvWrite state (Closed (count+1) limit)
                                   | otherwise     -> relief_open state valve wheelacc
        rte_exit excl

relief_close state valve = do
        rte_write valve False
        rte_irvWrite state (Closed 0 5)

relief_open state valve wheelacc = do
        rte_write valve True
        Ok a <- rte_call wheelacc ()
        rte_irvWrite state (if a < 0 then Open 0 (-a*10) else Closed 0 5)

relief_stop excl state valve () = do
        rte_enter excl
        rte_write valve False
        rte_irvWrite state Stopped
        rte_exit excl
        return ()

relief_start excl state valve wheelacc () = do
        rte_enter excl
        relief_open state valve wheelacc
        rte_exit excl
        return ()


relief = do
        excl <- exclusiveArea
        state <- interRunnableVariable Stopped
        valve <- providedDataElement
        wheelacc <- requiredOperation
        runnable (MinInterval 0.0) [Timed 0.001] (relief_tick excl state valve wheelacc)
        start <- providedOperation
        stop <- providedOperation
        serverRunnable (MinInterval 0) [start] (relief_start excl state valve wheelacc)
        serverRunnable (MinInterval 0) [stop] (relief_stop excl state valve)
        return (seal valve, seal wheelacc, seal start, seal stop)

{-
pressure_tick state valve wheelacc = do
        Ok s <- rte_irvRead state
        case s of
                Stopped                            -> rte_irvWrite state s
                Open count limit   | count < limit -> rte_irvWrite state (Open (count+1) limit)
                                   | otherwise     -> pressure_close state valve
                Closed count limit | count < limit -> rte_irvWrite state (Closed (count+1) limit)
                                   | otherwise     -> pressure_open state valve wheelacc

pressure_close state valve = do
        rte_write valve False
        rte_irvWrite state (Closed 0 20)

pressure_open state valve wheelacc = do
        rte_write valve True
        Ok a <- rte_call wheelacc ()
        rte_irvWrite state (if a > 0 then Open 0 (a*50) else Stopped)

pressure_stop state valve = do
        rte_write valve False
        rte_irvWrite state Stopped

pressure_start state valve wheelacc = do
        rte_write valve True
        rte_irvWrite state (Open 0 100)

-}