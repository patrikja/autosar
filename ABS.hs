module ABS where
        
import ARSim
import System.Random

instance (Valuable a, Valuable b) => Valuable (a,b) where
        toVal (a,b)             = VArray [toVal a, toVal b]
        fromVal (VArray [a, b]) = (fromVal a, fromVal b)

data SeqState = Stopped | Running Int Int Int

instance Valuable SeqState where
        toVal Stopped           = Void
        toVal (Running t l a)   = VArray [VInt t, VInt l, VInt a]
        fromVal Void            = Stopped
        fromVal (VArray [VInt t, VInt l, VInt a])
                                = Running t l a
        
seq_tick step excl state = do
        rte_enter excl
        Ok s <- rte_irvRead state
        s' <- case s of
                Stopped                 -> return s
                Running ticks limit a 
                        | ticks < limit -> return (Running (ticks+1) limit a)
                        | otherwise     -> step a
        rte_irvWrite state s'
        rte_exit excl

seq_onoff step excl state on = do
        rte_enter excl
        s <- step (if on then 0 else -1)
        rte_irvWrite state s
        rte_exit excl
        return ()

sequencer step = do
        excl <- exclusiveArea
        state <- interRunnableVariable Stopped
        runnable (MinInterval 0) [Timed 0.001] (seq_tick step excl state)
        onoff <- providedOperation
        serverRunnable (MinInterval 0) [onoff] (seq_onoff step excl state)
        return (seal onoff)



relief_step valve accel 0 = do
        Ok a <- rte_read accel
        if a < 0 then do
                rte_write valve True
                return (Running 0 (-(round a)*10) 1)
         else
                return (Running 0 5 0)
relief_step valve accel 1 = do
        rte_write valve False
        return (Running 0 5 0)
relief_step valve accel n = do
        rte_write valve False
        return Stopped

relief :: AR c (RE Double (), PO Bool () (), PE Bool ())
relief = component $ do
        valve <- providedDataElement
        accel <- requiredDataElement
        ctrl <- sequencer $ relief_step valve accel
        return (seal accel, ctrl, seal valve)

pressure_step valve accel 0 = do
        rte_write valve True
        return (Running 0 100 1)
pressure_step valve accel 20 = do
        rte_write valve True
        return Stopped
pressure_step valve accel n | even n = do
        rte_write valve True
        Ok a <- rte_read accel
        return (Running 0 (round a*50) (n+1))
pressure_step valve accel n | odd n = do
        rte_write valve False
        return (Running 0 20 (n+1))
pressure_step valve accel n | n < 0 = do
        rte_write valve False
        return Stopped

pressure :: AR c (RE Double (), PO Bool () (), PE Bool ())
pressure = component $ do
        valve <- providedDataElement
        accel <- requiredDataElement
        ctrl <- sequencer $ pressure_step valve accel
        return (seal accel, ctrl, seal valve)


control memo onoff_pressure onoff_relief slipstream = do
        Ok slip <- rte_receive slipstream
        Ok slip' <- rte_irvRead memo
        case (slip < 0.8, slip' < 0.8) of
                (True, False) -> do
                        rte_call onoff_pressure False
                        rte_call onoff_relief True
                        return ()
                (False, True) -> do
                        rte_call onoff_relief False
                        rte_call onoff_pressure True
                        return ()
                _ ->    return ()
        rte_irvWrite memo slip

controller :: AR c (RQ Double (), RO Bool () (), RO Bool () ())
controller = component $ do
        memo <- interRunnableVariable 1.0
        onoff_pressure <- requiredOperation
        onoff_relief <- requiredOperation
        slipstream <- requiredQueueElement 10
        runnable (MinInterval 0) [ReceiveQ slipstream] 
                (control memo onoff_pressure onoff_relief slipstream)
        return (seal slipstream, seal onoff_pressure, seal onoff_relief)

loop velostreams slipstreams = do
        velos <- mapM (\re -> do Ok v <- rte_read re; return v) velostreams
        let v0 = minimum velos
        mapM (\(v,pe) -> rte_send pe (v/v0)) (velos `zip` slipstreams)

main_loop :: AR c ([RE Double ()], [PQ Double ()])
main_loop = component $ do
        velostreams <- mapM (const requiredDataElement) [1..4]
        slipstreams <- mapM (const providedQueueElement) [1..4]
        runnable (MinInterval 0) [Timed 0.01] (loop velostreams slipstreams)
        return (map seal velostreams, map seal slipstreams)
        
abs_system = do
        (velostreams_in, slipstreams_out) <- main_loop
        (slipstreams_in, onoffs_r_out, onoffs_p_out) <- fmap unzip3 $ mapM (const controller) [1..4]
        (accels_r_in, onoffs_r_in, valves_r_out) <- fmap unzip3 $ mapM (const relief) [1..4]
        (accels_p_in, onoffs_p_in, valves_p_out) <- fmap unzip3 $ mapM (const pressure) [1..4]
        
        connectAll slipstreams_out slipstreams_in
        connectAll onoffs_r_out onoffs_r_in
        connectAll onoffs_p_out onoffs_p_in
        
        return (velostreams_in, accels_r_in, accels_p_in, valves_r_out, valves_p_out)
        
test vel_sim acc_sim = do
        (velos_in, accels_r_in, accels_p_in, valves_r_out, valves_p_out) <- abs_system

        v_sensors <- sources vel_sim [1..4]
        a_sensors <- sources acc_sim [1..4]
        connectAll v_sensors velos_in
        connectAll a_sensors accels_r_in
        connectAll a_sensors accels_p_in
        
        r_actuators <- sinks [1..4]
        p_actuators <- sinks [1..4]
        connectAll valves_r_out r_actuators
        connectAll valves_p_out p_actuators

sources table nums =
        mapM (source . table) nums

sinks nums =
        mapM (const sink) nums

