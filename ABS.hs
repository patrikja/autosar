module Main where
        
import ARSim
import System.Random

instance (Valuable a, Valuable b) => Valuable (a,b) where
        toVal (a,b)             = VArray [toVal a, toVal b]
        fromVal (VArray [a, b]) = (fromVal a, fromVal b)


--------------------------------------------------------------
-- A generic skeleton for creating sequencer components; i.e., 
-- components that produce sequences of observable effects at
-- programmable points in time. The skeleton is parametric in 
-- the actual step function (which takes takes a step index as
-- a parameter, performs any observable effects, and returns a
-- new sequencer state). A sequencer state is either Stopped or 
-- (Running ticks limit index), where limit is the step size in
-- milliseconds, ticks is the time in milliseconds since the 
-- last step, and index is the new step index. A sequencer 
-- can be started and stopped by means of an exported boolean
-- service operation.
--------------------------------------------------------------

data SeqState = Stopped | Running Int Int Int

instance Valuable SeqState where
        toVal Stopped           = Void
        toVal (Running t l i)   = VArray [VInt t, VInt l, VInt i]
        fromVal Void            = Stopped
        fromVal (VArray [VInt t, VInt l, VInt i])
                                = Running t l i
        
seq_tick step excl state = do
        rte_enter excl
        Ok s <- rte_irvRead state
        s' <- case s of
                Stopped                   -> return s
                Running ticks limit i 
                        | ticks < limit-1 -> return (Running (ticks+1) limit i)
                        | otherwise       -> step i
        rte_irvWrite state s'
        rte_exit excl

seq_onoff onoff excl state on = do
        rte_enter excl
        s <- onoff on
        rte_irvWrite state s
        rte_exit excl
        return ()

sequencer step ctrl = do
        excl <- exclusiveArea
        state <- interRunnableVariable Stopped
        runnable (MinInterval 0) [Timed 0.001] (seq_tick step excl state)
        onoff <- providedOperation
        serverRunnable (MinInterval 0) [onoff] (seq_onoff ctrl excl state)
        return (seal onoff)

--------------------------------------------------------------
-- A relief component is a sequencer for toggling a brake relief
-- valve in an ABS system. It requires a data element for reading
-- wheel acceleration values (of type Double) and pulses the valve
-- in lengths proportional to acceleration, as long as this value
-- stays negative. It provides a boolean on/off control operation
-- as well as a boolean data element producing valve settings.
--------------------------------------------------------------

relief_step valve accel 0 = do
        Ok a <- rte_read accel
        if a < 0 then do
                rte_write valve True
                return (Running 0 (round (-a*10)) 1)
         else
                return (Running 0 5 0)
relief_step valve accel n = do
        rte_write valve False
        return (Running 0 5 0)

relief_ctrl valve accel True = 
        relief_step valve accel 0
relief_ctrl valve accel False = do
        rte_write valve False
        return Stopped
        

relief :: AR c (RE Double (), PO Bool () (), PE Bool ())
relief = component $ do
        valve <- providedDataElement
        accel <- requiredDataElement
        ctrl <- sequencer (relief_step valve accel) (relief_ctrl valve accel)
        return (seal accel, ctrl, seal valve)

--------------------------------------------------------------
-- A pressure component is a sequencer for toggling a brake
-- pressure valve in an ABS system. It requires a data element
-- for reading wheel acceleration values (of type Double) and 
-- pulses the valve 10 times in lengths proportional to positive
-- acceleration. It provides a boolean on/off control operation
-- as well as a boolean data element producing valve settings.
--------------------------------------------------------------

pressure_step valve accel 0 = do
        rte_write valve True
        return (Running 0 100 1)
pressure_step valve accel 20 = do
        rte_write valve True
        return Stopped
pressure_step valve accel n | even n = do
        rte_write valve True
        Ok a <- rte_read accel
        return (Running 0 (round (a*50)) (n+1))
pressure_step valve accel n | odd n = do
        rte_write valve False
        return (Running 0 20 (n+1))
pressure_step valve accel n | n < 0 = do
        rte_write valve False
        return Stopped

pressure_ctrl valve True =
        return (Running 0 0 0)
pressure_ctrl valve False = do
        rte_write valve False
        return Stopped

pressure :: AR c (RE Double (), PO Bool () (), PE Bool ())
pressure = component $ do
        valve <- providedDataElement
        accel <- requiredDataElement
        ctrl <- sequencer (pressure_step valve accel) (pressure_ctrl valve)
        return (seal accel, ctrl, seal valve)

--------------------------------------------------------------
-- A controller component reads a stream of slip values for a
-- wheel (that is, ratios between wheel and vehicle speeds), and
-- watches for points where slip drops below or rises above 80%.
-- If slip drops below 80%, a relief sequence for the wheel is
-- started (after any ongoing pressure sequence is aborted).
-- If slip rises above 80%, any ongoing relief sequence is aborted
-- and a pressure sequence is started.
--------------------------------------------------------------

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

--------------------------------------------------------------
-- The "main loop" of the ABS algorithm is a component that
-- periodically reads the current speeds of all wheels, 
-- approximates the vehicle speed as the maximum of the wheel
-- speeds (should be refined for the case when all wheels are
-- locked, must resort do dead reckoning based on latest 
-- deacceleration in these cases), and sends every wheel 
-- controller its updated slip ratio.
--------------------------------------------------------------

loop velostreams slipstreams = do
        velos <- mapM (\re -> do Ok v <- rte_read re; return v) velostreams
        let v0 = maximum velos
        mapM (\(v,pe) -> rte_send pe (v/v0)) (velos `zip` slipstreams)

main_loop :: AR c ([RE Double ()], [PQ Double ()])
main_loop = component $ do
        velostreams <- mapM (const requiredDataElement) [1..4]
        slipstreams <- mapM (const providedQueueElement) [1..4]
        runnable (MinInterval 0) [Timed 0.01] (loop velostreams slipstreams)
        return (map seal velostreams, map seal slipstreams)
        
--------------------------------------------------------------
-- A full ABS system consists of a main loop component as well
-- as one controller, one relief and one pressure component for 
-- each wheel. In addition, each wheel must be equipped with
-- speed and acceleration sensors, but these parts are considered
-- external to the current system.
--------------------------------------------------------------

abs_system = do
        (velostreams_in, slipstreams_out) <- main_loop
        (slipstreams_in, onoffs_r_out, onoffs_p_out) <- fmap unzip3 $ mapM (const controller) [1..4]
        (accels_r_in, onoffs_r_in, valves_r_out) <- fmap unzip3 $ mapM (const relief) [1..4]
        (accels_p_in, onoffs_p_in, valves_p_out) <- fmap unzip3 $ mapM (const pressure) [1..4]
        
        connectAll slipstreams_out slipstreams_in
        connectAll onoffs_r_out onoffs_r_in
        connectAll onoffs_p_out onoffs_p_in
        
        return (velostreams_in, accels_r_in, accels_p_in, valves_r_out, valves_p_out)

--------------------------------------------------------------
-- A test setup consists of the ABS system connected to simulated
-- data sources in place of wheel sensors, and data sinks in place
-- of pressure and relief valves.
--------------------------------------------------------------

test vel_sim acc_sim = do
        (velos_in, accels_r_in, accels_p_in, valves_r_out, valves_p_out) <- abs_system

        v_sensors <- mapM (source . vel_sim) [1..4]
        a_sensors <- mapM (source . acc_sim) [1..4]
        connectAll v_sensors velos_in
        connectAll a_sensors accels_r_in
        connectAll a_sensors accels_p_in
        
        r_actuators <- mapM (const sink) [1..4]
        p_actuators <- mapM (const sink) [1..4]
        connectAll valves_r_out r_actuators
        connectAll valves_p_out p_actuators
        
        return $ tag v_sensors ++ tag a_sensors ++ tag r_actuators ++ tag p_actuators

simulation vel_sim acc_sim = do 
        s <- newStdGen
        let (trace,tags) = simulationRand s (test vel_sim acc_sim)
        putTraceLabels trace

--main = simulation (const [(0,0)]) (const [(0,0)])
main = seq_sim

ctrl = component $ do
        trig <- requiredDataElement
        op <- requiredOperation
        runnable (MinInterval 0) [ReceiveE trig] (rte_call op True)
        return (seal trig, seal op)

seq_test = do
        (accel_in, onoff_in, valve_out) <- relief
        (trig_in,onoff_out) <- ctrl
        
        trig_sens <- source [(0.0, True)]
        accel_sens <- source vs
        valve <- sink
        connect trig_sens trig_in
        connect accel_sens accel_in
        connect onoff_out onoff_in
        connect valve_out valve
        
        return valve

k1 = 1.9
k2 = 0.02
xs = [0.0,k2..1.0]
f t = 0.5 - k1*((1.0-t)^2)
vs = (0 : repeat k2) `zip` map f xs

seq_sim = do
        s <- newStdGen
        let (trace, valve) = simulationRand s seq_test
            trace' = chopTrace 5000 trace
        print (absolute 0.0 (collect trace' valve))
        putStr "\n"
        print (absolute 0.0 vs)
--        putTraceLabels trace'

absolute base [] = []
absolute base ((t,v):vs) = (round ((base+t)*1000),v) : absolute (base+t) vs