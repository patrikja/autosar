module Main where
        
import ARSim
import System.Random
import Graphics.EasyPlot
import Debug.Trace

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

seq_init setup excl state = do
        rte_enter excl
        s <- setup
        rte_irvWrite state s
        rte_exit excl

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

sequencer setup step ctrl = do
        excl <- exclusiveArea
        state <- interRunnableVariable Stopped
        runnable Concurrent [Init] (seq_init setup excl state)
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

relief_setup valve = do
        rte_write valve False
        return Stopped
        
relief_step valve accel 0 = do
        Ok a <- rte_read accel
        trace ("Relief " ++ show a) (return ())
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
        ctrl <- sequencer (relief_setup valve) (relief_step valve accel) (relief_ctrl valve accel)
        return (seal accel, ctrl, seal valve)

--------------------------------------------------------------
-- A pressure component is a sequencer for toggling a brake
-- pressure valve in an ABS system. It requires a data element
-- for reading wheel acceleration values (of type Double) and 
-- pulses the valve 10 times in lengths proportional to positive
-- acceleration. It provides a boolean on/off control operation
-- as well as a boolean data element producing valve settings.
--------------------------------------------------------------

pressure_setup valve = do
        rte_write valve True
        return Stopped

pressure_step valve accel 0 = do
        rte_write valve True
        return (Running 0 100 1)
pressure_step valve accel 20 = do
        rte_write valve True
        return Stopped
pressure_step valve accel n | even n = do
        rte_write valve True
        Ok a <- rte_read accel
        trace ("Pressure " ++ show a) (return ())
        return (Running 0 (round (a*50)) (n+1))
pressure_step valve accel n | odd n = do
        rte_write valve False
        return (Running 0 20 (n+1))

pressure_ctrl valve accel True =
        pressure_step valve accel 0
pressure_ctrl valve accel False = do
        rte_write valve False
        return Stopped

pressure :: AR c (RE Double (), PO Bool () (), PE Bool ())
pressure = component $ do
        valve <- providedDataElement
        accel <- requiredDataElement
        ctrl <- sequencer (pressure_setup valve) (pressure_step valve accel) (pressure_ctrl valve accel)
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
                        trace ("Slip " ++ show slip) (return ())
                        rte_call onoff_pressure False
                        rte_call onoff_relief True
                        return ()
                (False, True) -> do
                        trace ("Slip " ++ show slip) (return ())
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
        mapM (\(v,pe) -> rte_send pe (slip v0 v)) (velos `zip` slipstreams)

slip 0.0 v      = 1.0
slip v0 v       = v/v0

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
        (slipstreams_in, onoffs_p_out, onoffs_r_out) <- fmap unzip3 $ mapM (const controller) [1..4]
        (accels_p_in, onoffs_p_in, valves_p_out) <- fmap unzip3 $ mapM (const pressure) [1..4]
        (accels_r_in, onoffs_r_in, valves_r_out) <- fmap unzip3 $ mapM (const relief) [1..4]
        
        connectAll slipstreams_out slipstreams_in
        connectAll onoffs_p_out onoffs_p_in
        connectAll onoffs_r_out onoffs_r_in
        
        return (velostreams_in, accels_r_in, accels_p_in, valves_r_out, valves_p_out)

--------------------------------------------------------------
-- A test setup consists of the ABS system connected to simulated
-- data sources in place of wheel sensors, and data sinks in place
-- of pressure and relief valves.
--------------------------------------------------------------

test vel_sim acc_sim = do
        (velos_in, accels_r_in, accels_p_in, valves_r_out, valves_p_out) <- abs_system

        v_sensors <- mapM source vel_sim
        a_sensors <- mapM source acc_sim
        connectAll v_sensors velos_in
        connectAll a_sensors accels_r_in
        connectAll a_sensors accels_p_in
        
        r_actuators <- mapM (const sink) [1..4]
        p_actuators <- mapM (const sink) [1..4]
        connectAll valves_r_out r_actuators
        connectAll valves_p_out p_actuators
        
        return (r_actuators, p_actuators)

curve1 = slopes [(0,18),(1,18),(5,0)]

curve2 = slopes [(0,18),(1,18),(1.8,10),(2.5,7),(4,4.5),(4.5,0)]

v1 = curve1
v2 = curve2  -- Let wheel 2 speed deviate
v3 = curve1
v4 = curve1

a1 = deriv v1
a2 = deriv v2
a3 = deriv v3
a4 = deriv v4

abs_sim = do 
        s <- newStdGen
        let (trace,(r_acts,p_acts)) = chopTrace 400000 $ simulationRand s (test [v1,v2,v3,v4] [a1,a2,a3,a4])
            [r1,r2,r3,r4] = map (discrete . bool2num 1.0 . collect trace) r_acts
            [p1,p2,p3,p4] = map (discrete . bool2num 1.0 . collect trace) p_acts
        plot (PS "plot.ps") $ [Data2D [Title "pressure 2", Style Lines, Color Red] [] p2,
                               Data2D [Title "relief 2", Style Lines, Color Blue] [] r2,
                               Data2D [Title "wheel speed 2", Style Lines, Color Green] [] v2,
                               Data2D [Title "wheel acceleration 2", Style Lines, Color Violet] [] (discrete a2),
                               Data2D [Title "vehicle speed", Style Lines, Color Black] [] v1]
--        putTraceLabels trace

main = abs_sim
---main = seq_sim

-----------------------------------------------------------------
-- A simple test of a single relief sequencer
-----------------------------------------------------------------

ctrl = component $ do
        trig <- requiredDataElement
        op <- requiredOperation
        runnable (MinInterval 0) [ReceiveE trig] (rte_call op True)
        return (seal trig, seal op)

seq_test accel_curve = do
        (accel_in, onoff_in, valve_out) <- relief
        (trig_in,onoff_out) <- ctrl
        
        trig_sens <- source [(0.1, True)]
        accel_sens <- source accel_curve
        valve <- sink
        connect trig_sens trig_in
        connect accel_sens accel_in
        connect onoff_out onoff_in
        connect valve_out valve
        
        return valve

curve0          = slopes (xs `zip` map f xs)
  where k1      = 3.0
        k2      = 0.1
        xs      = [0.0,k2..0.6]
        f t     = 0.5 - k1*((1.0-t)^2)

seq_sim = do
        s <- newStdGen
        let (trace, valve) = chopTrace 5000 $ simulationRand s (seq_test curve0)
            out = discrete $ bool2num 1.0 $ collect trace valve
        plot (PS "plot.ps") $ [Data2D [Title "relief", Style Lines, Color Blue] [] out,
                               Data2D [Title "accel", Style Lines, Color Red] [] curve0]
--        putTraceLabels trace

--------------------------------------------------
-- Some helper functions for generating test data
--------------------------------------------------

slopes ((x,y):pts@((x',y'):_))
  | x >= x'                     = slopes pts
  | y == y'                     = (x,y) : slopes pts
  | otherwise                   = slope x y (dx*(y'-y)/(x'-x)) pts
  where dx                      = 0.01
        slope x y dy pts@((x',y'):_)
          | x < x'-(dx/2)       = (x,y) : slope (x+dx) (y+dy) dy pts
        slope x y dy pts        = slopes pts
slopes pts                      = pts


discrete vs                     = disc 0.0 vs
  where disc v0 ((t,v):vs)      = (t,v0) : (t+eps,v) : disc v vs
        disc _ _                = []
        eps                     = 0.0001

deriv ((x,y):pts@((x',y'):_))
  | x >= x'                     = deriv pts
  | otherwise                   = (x, (y'-y)/(x'-x)) : deriv pts
deriv [(x,y)]                   = []
deriv []                        = []

bool2num k                      = map (\(t,b) -> (t,if b then k else 0.0))

