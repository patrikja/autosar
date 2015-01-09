{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import NewARSim
import qualified Data.Map as Map
import Data.Dynamic
import Graphics.EasyPlot
import System.Random

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

data SeqState = Stopped | Running Int Int Int deriving Typeable

seq_init setup excl state = do
        rteEnter excl
        s <- setup
        rteIrvWrite state s
        rteExit excl

seq_tick step excl state = do
        rteEnter excl
        Ok s <- rteIrvRead state
        s' <- case s of
                Stopped                   -> return s
                Running ticks limit i 
                        | ticks < limit-1 -> return (Running (ticks+1) limit i)
                        | otherwise       -> step i
        rteIrvWrite state s'
        rteExit excl

seq_onoff onoff excl state on = do
        rteEnter excl
        s <- onoff on
        rteIrvWrite state s
        rteExit excl
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
        rteWrite valve False
        return Stopped
        
relief_step valve accel 0 = do
        Ok a <- rteRead accel
--        trace ("Relief " ++ show a) (return ())
        if a < 0 then do
                rteWrite valve True
                return (Running 0 (round (-a*10)) 1)
         else
                return (Running 0 5 0)
relief_step valve accel n = do
        rteWrite valve False
        return (Running 0 5 0)

relief_ctrl valve accel True = 
        relief_step valve accel 0
relief_ctrl valve accel False = do
        rteWrite valve False
        return Stopped
        
relief_seq :: AR c (RequiredDataElement Double (), ProvidedOperation Bool () (), ProvidedDataElement Bool ())
relief_seq = component $ do
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
        rteWrite valve True
        return Stopped

pressure_step valve accel 0 = do
        rteWrite valve True
        return (Running 0 100 1)
pressure_step valve accel 20 = do
        rteWrite valve True
        return Stopped
pressure_step valve accel n | even n = do
        rteWrite valve True
        Ok a <- rteRead accel
--        trace ("Pressure " ++ show a) (return ())
        return (Running 0 (round (a*50)) (n+1))
pressure_step valve accel n | odd n = do
        rteWrite valve False
        return (Running 0 20 (n+1))

pressure_ctrl valve accel 2 = do
        rteWrite valve True
        return Stopped
pressure_ctrl valve accel 1 =
        pressure_step valve accel 0
pressure_ctrl valve accel 0 = do
        rteWrite valve False
        return Stopped

pressure_seq :: AR c (RequiredDataElement Double (), ProvidedOperation Int () (), ProvidedDataElement Bool ())
pressure_seq = component $ do
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
        Ok slip <- rteReceive slipstream
        Ok slip' <- rteIrvRead memo
        case (slip < 0.8, slip' < 0.8) of
                (True, False) -> do
--                        trace ("Slip " ++ show slip) (return ())
                        rteCall onoff_pressure 0
                        rteCall onoff_relief True
                        return ()
                (False, True) -> do
--                        trace ("Slip " ++ show slip) (return ())
                        rteCall onoff_relief False
                        rteCall onoff_pressure (if slip >= 1.0 then 2 else 1)
                        return ()
                _ ->    return ()
        rteIrvWrite memo slip

controller :: AR c (RequiredQueueElement Double (), RequiredOperation Int () (), RequiredOperation Bool () ())
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
        velos <- mapM (\re -> do Ok v <- rteRead re; return v) velostreams
        let v0 = maximum velos
        mapM (\(v,pe) -> rteSend pe (slip v0 v)) (velos `zip` slipstreams)

slip 0.0 v      = 1.0
slip v0 v       = v/v0

main_loop :: AR c ([RequiredDataElement Double ()], [ProvidedQueueElement Double ()])
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

wheel_ctrl slipstream = component $ do
        (slip, onoff_pressure, onoff_relief) <- controller
        (accel_p, ctrl_p, valve_p) <- pressure_seq
        (accel_r, ctrl_r, valve_r) <- relief_seq
        connect slipstream slip
        connect onoff_pressure ctrl_p
        connect onoff_relief ctrl_r
        return (accel_r, accel_p, valve_r, valve_p)

abs_system = do
        (velos_in, slips_out) <- main_loop
        ifaces <- mapM wheel_ctrl slips_out
        return (velos_in, ifaces)


-------------------------------------------------------------------
--  A simulated physical car
-------------------------------------------------------------------

wheel_f time pressure relief velo 
        | time < 1.0                = (0::Double, velo)
        | pressure && not relief    = (-10, velo-0.1)
        | relief && not pressure    = (1, velo+0.01)
        | otherwise                 = (-3, velo-0.03)

wheel_sim t ((r_act, p_act, v_sens, a_sens), velo) = do
        Ok pressure <- rteRead p_act
        Ok relief <- rteRead r_act
        let (acc,velo1) = wheel_f t pressure relief velo
        rteWrite v_sens velo1
        rteWrite a_sens acc
        return velo1


simul wheels irv = do
        Ok (time, velos) <- rteIrvRead irv
        let t = time + 0.01
        velos1 <- mapM (wheel_sim t) (wheels `zip` velos)
        rteIrvWrite irv (t, velos1)
        return ()

mk_wheel i = do
        r_act <- requiredDataElement
        p_act <- requiredDataElement
        v_sens <- providedDataElement
        a_sens <- providedDataElement
        if i==1 then do
            probePE "vehicle speed" v_sens id
         else if i==2 then do
            probeRE "relief 2"    r_act ((+2.0) . fromIntegral . fromEnum)
            probeRE "pressure 2"  p_act ((+5.0) . fromIntegral . fromEnum)
            probePE "wheel speed 2"         v_sens id
            probePE "wheel acceleration 2"  a_sens id
         else return ()
        return (r_act, p_act, v_sens, a_sens)

car = do
        wheels <- mapM mk_wheel [1..4]
        irv <- interRunnableVariable (0::Double, replicate 4 (18::Double))
        runnable (MinInterval 0) [Timed 0.01] (simul wheels irv)
        return (map seal4 wheels)

-------------------------------------------------------------------------
-- A test setup consists of the ABS implementation and the simulated car 
-- cyclically connected into a closed system.
--------------------------------------------------------------------------

test = do
        (velos_in, ifaces) <- abs_system
        wheels <- car
        sequence (zipWith3 conn wheels velos_in ifaces)
    
conn (r_act,p_act,v_sens,a_sens) velo_in (accel_r,accel_p,valve_r,valve_p) = do
        connect v_sens velo_in
        connect a_sens accel_r
        connect a_sens accel_p
        connect valve_r r_act
        connect valve_p p_act

main1 = makePlot (runARSim TrivialSched 5.0 test)

main2 = do g <- getStdGen
           makePlot (runARSim (RandomSched g) 5.0 test)

makePlot meas = plot (PS "plot.ps") curves
  where curves  = [ Data2D [Title str, Style Lines, Color (color str)] [] (discrete pts)
                  | (str,pts) <- meas ]
        color "pressure 2"              = Red
        color "relief 2"                = Blue
        color "wheel speed 2"           = Green
        color "wheel acceleration 2"    = Violet
        color _                         = Black
        discrete []                     = []
        discrete ((t,v):vs)             = (t,v) : disc v vs
          where disc v0 ((t,v):vs)      = (t,v0) : (t+eps,v) : disc v vs
                disc _ _                = []
                eps                     = 0.0001

main = main2
