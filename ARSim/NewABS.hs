{-
Copyright (c) 2014-2015, Johan Nordlander, Jonas Duregård, Michał Pałka,
                         Patrik Jansson and Josef Svenningsson
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
   * Neither the name of the Chalmers University of Technology nor the names of its 
     contributors may be used to endorse or promote products derived from this 
     software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import NewARSim
import Control.Monad
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
type Ticks = Int
type Limit = Int
type Index = Int
data SeqState = Stopped | Running Ticks Limit Index deriving (Typeable,Data)

seq_init :: (Data a) =>
  RTE c a -> ExclusiveArea c -> InterRunnableVariable a c -> RTE c (StdRet ())
seq_init setup excl state = do
        rteEnter excl
        s <- setup
        rteIrvWrite state s
        rteExit excl

seq_tick ::  (Index -> RTE c SeqState) -> ExclusiveArea c -> InterRunnableVariable SeqState c
         ->  RTE c (StdRet ())
seq_tick step excl state = do
        rteEnter excl
        Ok s <- rteIrvRead state
        s' <- case s of
                Stopped                    -> return s
                Running ticks limit i 
                        | ticks < limit-1  -> return (Running (ticks+1) limit i)
                        | otherwise        -> step i
        rteIrvWrite state s'
        rteExit excl

seq_onoff :: (Data a) =>
     (t -> RTE c a) -> ExclusiveArea c -> InterRunnableVariable a c
  -> (t -> RTE c ())
seq_onoff onoff excl state on = do
        rteEnter excl
        s <- onoff on
        rteIrvWrite state s
        rteExit excl
        return ()

sequencer :: Data a =>
  RTE c SeqState -> (Index -> RTE c SeqState) -> (a -> RTE c SeqState)
  -> AR c (ClientServerOperation a () Provided c)
sequencer setup step ctrl = do
        excl <- exclusiveArea
        state <- interRunnableVariable Stopped
        runnable Concurrent [InitEvent]               (seq_init setup excl state)
        runnable (MinInterval 0) [TimingEvent 0.001]  (seq_tick step  excl state)
        onoff <- provide ServerComSpec{bufferLength=0}
        serverRunnable (MinInterval 0) [OperationInvokedEvent onoff]  (seq_onoff ctrl excl state)
        return (onoff)

--------------------------------------------------------------
-- A relief component is a sequencer for toggling a brake relief valve
-- in an ABS system. It requires a data element for reading wheel
-- acceleration values (of type Accel = Double) and pulses the valve
-- in lengths proportional to acceleration, as long as this value
-- stays negative. It provides a boolean on/off control operation as
-- well as a boolean data element producing valve settings.
--------------------------------------------------------------

relief_setup :: DataElement Unqueued Bool Provided c -> RTE c SeqState
relief_setup valve = do
        rteWrite valve False
        return Stopped
        
relief_step ::  DataElement Unqueued Valve Provided c -> 
                DataElement Unqueued Accel Required c -> 
                Index -> RTE c SeqState
relief_step valve accel 0 = do
        Ok a <- rteRead accel
        printlog "" ("Relief " ++ show a)
        if a < 0 then do
                rteWrite valve True
                return (Running 0 (round (-a*10)) 1)
         else
                return (Running 0 5 0)
relief_step valve accel n = do
        rteWrite valve False
        return (Running 0 5 0)

relief_ctrl ::  DataElement Unqueued Valve Provided c   -> 
                DataElement Unqueued Accel Required c   -> 
                Valve -> RTE c SeqState
relief_ctrl valve accel True = 
        relief_step valve accel 0
relief_ctrl valve accel False = do
        rteWrite valve False
        return Stopped
        
type Relief = ( DataElement Unqueued Accel Required (),
                ClientServerOperation Bool () Provided (),
                DataElement Unqueued Valve Provided ())
relief_seq :: AR c Relief
relief_seq = component $ do
        valve <- provide UnqueuedSenderComSpec{initSend=Nothing}
        accel <- require UnqueuedReceiverComSpec{initValue=Nothing}
        ctrl <- sequencer (relief_setup valve) (relief_step valve accel) (relief_ctrl valve accel)
        return (seal accel, seal ctrl, seal valve)

--------------------------------------------------------------
-- A pressure component is a sequencer for toggling a brake
-- pressure valve in an ABS system. It requires a data element
-- for reading wheel acceleration values (of type Double) and 
-- pulses the valve 10 times in lengths proportional to positive
-- acceleration. It provides a boolean on/off control operation
-- as well as a boolean data element producing valve settings.
--------------------------------------------------------------

type PresSeq = ( DataElement Unqueued Accel Required (), 
                 ClientServerOperation Index () Provided (), 
                 DataElement Unqueued Valve Provided ())

pressure_setup :: DataElement Unqueued Valve Provided c -> RTE c SeqState
pressure_setup valve = do
        rteWrite valve True
        return Stopped

pressure_step :: 
  DataElement Unqueued Valve Provided c  -> 
  DataElement Unqueued Accel Required c  -> 
  Index -> RTE c SeqState
pressure_step valve accel 0 = do
        rteWrite valve True
        return (Running 0 100 1)
pressure_step valve accel 20 = do
        rteWrite valve True
        return Stopped
pressure_step valve accel n | even n = do
        rteWrite valve True
        Ok a <- rteRead accel
        printlog "" ("Pressure " ++ show a)
        return (Running 0 (round (a*50)) (n+1))
pressure_step valve accel n | odd n = do
        rteWrite valve False
        return (Running 0 20 (n+1))

pressure_ctrl :: 
  DataElement Unqueued Valve Provided c  -> 
  DataElement Unqueued Accel Required c  -> 
  Index -> RTE c SeqState
pressure_ctrl valve accel 2 = do
        rteWrite valve True
        return Stopped
pressure_ctrl valve accel 1 =
        pressure_step valve accel 0
pressure_ctrl valve accel 0 = do
        rteWrite valve False
        return Stopped

pressure_seq :: AR c PresSeq
pressure_seq = component $ do
        valve <- provide UnqueuedSenderComSpec{initSend=Nothing}
        accel <- require UnqueuedReceiverComSpec{initValue=Nothing}
        ctrl <- sequencer (pressure_setup valve) (pressure_step valve accel) (pressure_ctrl valve accel)
        return (seal accel, seal ctrl, seal valve)

--------------------------------------------------------------
-- A controller component reads a stream of slip values for a
-- wheel (that is, ratios between wheel and vehicle speeds), and
-- watches for points where slip drops below or rises above 80%.
-- If slip drops below 80%, a relief sequence for the wheel is
-- started (after any ongoing pressure sequence is aborted).
-- If slip rises above 80%, any ongoing relief sequence is aborted
-- and a pressure sequence is started.
--------------------------------------------------------------

control ::
  (Data pres, Num pres,
   Data r1, 
   Data q1) =>
  InterRunnableVariable   Slip  c  -> 
  ClientServerOperation pres q1 Required c  -> 
  ClientServerOperation Bool r1 Required c  -> 
  DataElement Queued Slip Required c  -> 
  RTE c (StdRet ())
control memo onoff_pressure onoff_relief slipstream = do
        Ok slip   <- rteReceive slipstream
        Ok slip'  <- rteIrvRead memo
        case (slip < 0.8, slip' < 0.8) of
                (True, False) -> do
                        printlog "" ("Slip " ++ show slip)
                        rteCall onoff_pressure 0
                        rteCall onoff_relief True
                        return ()
                (False, True) -> do
                        printlog "" ("Slip " ++ show slip)
                        rteCall onoff_relief False
                        rteCall onoff_pressure (if slip >= 1.0 then 2 else 1)
                        return ()
                _ ->    return ()
        rteIrvWrite memo slip

type Slip = Double
type ABSstate = (DataElement Queued Slip Required (), 
                 ClientServerOperation Int () Required (), 
                 ClientServerOperation Bool () Required ())
controller :: AR c ABSstate
controller = component $ do
        memo <- interRunnableVariable 1.0
        onoff_pressure <- require ClientComSpec
        onoff_relief <- require ClientComSpec
        slipstream <- require QueuedReceiverComSpec{queueLength=10}
        runnable (MinInterval 0) [DataReceivedEvent slipstream] 
                (control memo onoff_pressure onoff_relief slipstream)
        return (seal slipstream, seal onoff_pressure, seal onoff_relief)

type Accel = Double
type Velo  = Double
type Valve = Bool

wheel_ctrl :: (Index, DataElement Queued Slip Provided ()) -> AR c WheelCtrl
wheel_ctrl (i,slipstream) = component $ do
        (slip, onoff_pressure, onoff_relief) <- controller
        (accel_p, ctrl_p, valve_p) <- pressure_seq
        (accel_r, ctrl_r, valve_r) <- relief_seq
        connect slipstream slip
        connect ctrl_p onoff_pressure 
        connect ctrl_r onoff_relief
        when (i==2) $ do
            probeWrite "relief 2"    valve_r
            probeWrite "pressure 2"  valve_p
        return (accel_r, accel_p, valve_r, valve_p)



--------------------------------------------------------------
-- The "main loop" of the ABS algorithm is a component that
-- periodically reads the current speeds of all wheels, 
-- approximates the vehicle speed as the maximum of the wheel
-- speeds (should be refined for the case when all wheels are
-- locked, must resort do dead reckoning based on latest 
-- deacceleration in these cases), and sends every wheel 
-- controller its updated slip ratio.
--------------------------------------------------------------

fromOk (Ok v) = v

loop :: [DataElement Unqueued Velo Required c] -> 
        [DataElement Queued Velo Provided c] ->
        RTE c [StdRet ()]
loop velostreams slipstreams = do
        velos <- mapM (liftM fromOk . rteRead) velostreams
        let v0 = maximum velos
        mapM (\(v,p) -> rteSend p (slip v0 v)) (velos `zip` slipstreams)

slip :: Double -> Double -> Double
slip 0.0  _v = 1.0   -- no slip
slip v0   v  = v/v0

main_loop :: AR c ([DataElement Unqueued Velo Required ()], [DataElement Queued Slip Provided ()])
main_loop = component $ do
        velostreams <- replicateM 4 (require UnqueuedReceiverComSpec{initValue=Nothing})
        slipstreams <- replicateM 4 (provide QueuedSenderComSpec)
        runnable (MinInterval 0) [TimingEvent 0.01] (loop velostreams slipstreams)
        return (map seal velostreams, map seal slipstreams)
        
--------------------------------------------------------------
-- A full ABS system consists of a main loop component as well
-- as one controller, one relief and one pressure component for 
-- each wheel. In addition, each wheel must be equipped with
-- speed and acceleration sensors, but these parts are considered
-- external to the current system.
--------------------------------------------------------------

abs_system :: AR c ([VelocityIn], [WheelCtrl])
abs_system = component $ do
        (velos_in, slips_out) <- main_loop
        wheelctrls <- mapM wheel_ctrl ([1..] `zip` slips_out)
        return (velos_in, wheelctrls)


-------------------------------------------------------------------
--  A simulated physical car
-------------------------------------------------------------------
wheel_f :: Index -> Time -> Bool -> Bool -> Velo -> (Accel, Velo)
wheel_f i time pressure relief velo 
        | time < 1.0                = veloStep 0          velo
        | i /= 2                    = veloStep (-4.5)     velo
        -- let wheel 2 skid...
        -- We "postulate" a reasonable approximation of the wheel speed (ignoring the actual valves). Ideally the whole car dynamics whould be in another module (in Simulink).
        | time < 1.6                = veloStep (-10)      velo
        | time < 2                  = veloStep (-4)       velo
        | time < 2.5                = veloStep (-3)       velo
        | time < 3                  = veloStep 0.0        velo
        | time < 3.4                = veloStep (-4.5)     velo
        | time < 4                  = veloStep (-5)       velo
        | time < 4.3                = veloStep (-8.4)     velo
        | time < 4.7                = veloStep (-4)       velo
        | otherwise                 = veloStep 0          velo
-- The "pressure logic" is something like this, but a propoer treatment need integration over time and knowledge of vehicle speed and other physical parameters.
--        | pressure && not relief    = veloStep (-10)      velo   
--        | relief && not pressure    = veloStep (-1)       velo
--        | otherwise                 = veloStep (-3)       velo

veloStep :: Accel -> Velo -> (Accel, Velo)
veloStep a v = (a, v + a*0.01)

type Wheel = Wheel' ()


type Wheel' c = (DataElement Unqueued Valve Required c,
                 DataElement Unqueued Valve Required c,
                 DataElement Unqueued Velo  Provided c,
                 DataElement Unqueued Accel Provided c)

wheel_sim :: Time -> (Index, Wheel' c, Velo) -> RTE c Velo
wheel_sim t (i, (r_act, p_act, v_sens, a_sens), velo) = do
        Ok pressure <- rteRead p_act
        Ok relief <- rteRead r_act
        let (acc,velo') = wheel_f i t pressure relief velo
        rteWrite v_sens velo'
        rteWrite a_sens acc
        return velo'

simul :: [Wheel' c] -> InterRunnableVariable (Time, [Velo]) c -> RTE c ()
simul wheels irv = do
        Ok (time, velos) <- rteIrvRead irv
        let t = time + 0.01
        velos' <- mapM (wheel_sim t) (zip3 [1..] wheels velos)
        rteIrvWrite irv (t, velos')
        return ()

mk_wheel :: Int -> AR c (Wheel' c)
mk_wheel i = do
        r_act  <- require UnqueuedReceiverComSpec{initValue=Just False}
        p_act  <- require UnqueuedReceiverComSpec{initValue=Just True}
        v_sens <- provide UnqueuedSenderComSpec{initSend=Just init_v}
        a_sens <- provide UnqueuedSenderComSpec{initSend=Just init_a}
        when (i<=2) $ do
            probeWrite ("wheel "++show i++" speed")         v_sens
            probeWrite ("wheel "++show i++" acceleration")  a_sens
        return (r_act, p_act, v_sens, a_sens)

car :: AR c [Wheel]
car = do
        wheels <- mapM mk_wheel [1..4]
        irv <- interRunnableVariable (init_a, replicate 4 init_v)
        runnable (MinInterval 0) [TimingEvent 0.01] (simul wheels irv)
        return (map seal4 wheels)

init_v :: Velo
init_v = 18

init_a :: Accel
init_a = 0

-------------------------------------------------------------------------
-- A test setup consists of the ABS implementation and the simulated car 
-- cyclically connected into a closed system.
--------------------------------------------------------------------------

test :: AR c ()
test = do
        (velos_in, wheelctrls) <- abs_system
        wheels <- car
        sequence_ (zipWith3 conn wheels velos_in wheelctrls)

type VelocityIn = DataElement Unqueued Velo Required ()
type WheelCtrl = (  DataElement Unqueued Accel Required (),
                    DataElement Unqueued Accel Required (),
                    DataElement Unqueued Valve Provided (),
                    DataElement Unqueued Valve Provided ())

conn :: Wheel -> VelocityIn -> WheelCtrl -> AR c ()
conn (r_act,p_act,v_sens,a_sens) velo_in (accel_r,accel_p,valve_r,valve_p) = do
        connect v_sens velo_in
        connect a_sens accel_r
        connect a_sens accel_p
        connect valve_r r_act
        connect valve_p p_act


makePlot :: Trace -> IO Bool
makePlot trace = plot (PS "plot.ps") curves
  where curves  = [ Data2D [Title str, Style Lines, Color (color str)] [] (discrete pts)
                  | (str,pts) <- ms ]
        color "pressure 2"              = Red
        color "relief 2"                = Blue
        color "wheel 2 speed"           = Green
        color "wheel 2 acceleration"    = Violet
        color _                         = Black
        ms                              = bools ++ doubles
        doubles                         = probeAll trace
        bools                           = map scale (probeAll trace)

discrete []                     = []
discrete (((_,t),v):vs)         = (t,v) : disc v vs
  where disc v0 (((_,t),v):vs)  = (t,v0) : (t+eps,v) : disc v vs
        disc _ _                = []
        eps                     = 0.0001

scale :: (ProbeID, Measurement Bool) -> (ProbeID, Measurement Double)
scale ("relief 2",m)    = ("relief 2",map (fmap scaleValve) m)
  where scaleValve      = (+2.0) . fromIntegral . fromEnum
scale ("pressure 2",m)  = ("pressure 2",map (fmap scaleValve) m)
  where scaleValve      = (+4.0) . fromIntegral . fromEnum

main1 :: IO Bool
main1 = printLogs trace >> makePlot trace
  where trace = limitTime 5.0 $ execSim (RandomSched (mkStdGen 111)) test


main :: IO Bool
main = main1

