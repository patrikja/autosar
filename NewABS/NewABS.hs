{-
Copyright (c) 2014-2016, Johan Nordlander, Jonas Duregård, Michał Pałka,
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
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

sequencer :: Data a =>
  RTE c SeqState -> (Index -> RTE c SeqState) -> (a -> RTE c SeqState)
  -> Atomic c (ClientServerOperation a () Provided c)
sequencer setup step ctrl = do
        excl <- exclusiveArea
        state <- interRunnableVariable Stopped
        runnable Concurrent [InitEvent] $ do
            rteEnter excl
            s <- setup
            rteIrvWrite state s
            rteExit excl
        runnable (MinInterval 0) [TimingEvent 0.001] $ do
            rteEnter excl
            Ok s <- rteIrvRead state
            s' <- case s of
                    Stopped                    -> return s
                    Running ticks limit i      -- Intended invariant: ticks < limit
                            | ticks+1 < limit  -> return (Running (ticks+1) limit i)
                            | otherwise        -> step i
            rteIrvWrite state s'
            rteExit excl
        onoff <- providedPort
        serverRunnable (MinInterval 0) [OperationInvokedEvent onoff] $ \on -> do
            rteEnter excl
            s <- ctrl on
            rteIrvWrite state s
            rteExit excl
            return ()
        return (onoff)

--------------------------------------------------------------
-- A relief component is a sequencer for toggling a brake relief valve
-- in an ABS system. It requires a data element for reading wheel
-- acceleration values (of type Accel = Double) and pulses the valve
-- in lengths proportional to acceleration, as long as this value
-- stays negative. It provides a boolean on/off control operation as
-- well as a boolean data element producing valve settings.
--------------------------------------------------------------
defaultSeqState :: SeqState
defaultSeqState = Running 0 5 0 -- Count to five time steps (ms), then index 0

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
                return defaultSeqState
relief_step valve accel n = do
        rteWrite valve False
        return defaultSeqState

relief_ctrl ::  DataElement Unqueued Valve Provided c   ->
                DataElement Unqueued Accel Required c   ->
                Valve -> RTE c SeqState
relief_ctrl valve accel True =
        relief_step valve accel 0
relief_ctrl valve accel False = do
        rteWrite valve False
        return Stopped

type Relief = ( DataElem Unqueued Accel Required,
                ClientServerOp Bool () Provided,
                DataElem Unqueued Valve Provided )

relief_seq :: AUTOSAR Relief
relief_seq = atomic $ do
        valve <- providedPort
        accel <- requiredPort
        ctrl <- sequencer (relief_setup valve)
                          (relief_step  valve accel)
                          (relief_ctrl  valve accel)
        return $ seal (accel, ctrl, valve)

--------------------------------------------------------------
-- A pressure component is a sequencer for toggling a brake
-- pressure valve in an ABS system. It requires a data element
-- for reading wheel acceleration values (of type Double) and
-- pulses the valve 10 times in lengths proportional to positive
-- acceleration. It provides a boolean on/off control operation
-- as well as a boolean data element producing valve settings.
--------------------------------------------------------------

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

type PresSeq = ( DataElem Unqueued Accel Required,
                 ClientServerOp Index () Provided,
                 DataElem Unqueued Valve Provided )

pressure_seq :: AUTOSAR PresSeq
pressure_seq = atomic $ do
        valve <- providedPort
        accel <- requiredPort
        ctrl <- sequencer (pressure_setup valve)
                          (pressure_step  valve accel)
                          (pressure_ctrl  valve accel)
        return $ seal (accel, ctrl, valve)

--------------------------------------------------------------
-- A controller component reads a stream of slip values for a
-- wheel (that is, ratios between wheel and vehicle speeds), and
-- watches for points where slip drops below or rises above 80%.
-- If slip drops below 80%, a relief sequence for the wheel is
-- started (after any ongoing pressure sequence is aborted).
-- If slip rises above 80%, any ongoing relief sequence is aborted
-- and a pressure sequence is started.
--------------------------------------------------------------

type Slip = Double
data Controller = Controller {
            slipstream     :: DataElem Queued Slip Required,
            onoff_pressure :: ClientServerOp Int  () Required,
            onoff_relief   :: ClientServerOp Bool () Required  }

controller :: AUTOSAR Controller
controller = atomic $ do
        memo <- interRunnableVariable 1.0
        slipstream     <- requiredPort
        onoff_pressure <- requiredPort
        onoff_relief   <- requiredPort
        comSpec slipstream (QueueLength 10)
        runnable (MinInterval 0) [DataReceivedEvent slipstream] $ do
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
        return $ sealBy Controller slipstream onoff_pressure onoff_relief

type Accel = Double
type Velo  = Double
type Valve = Bool

data ValvePort r c = ValvePort {
        relief   :: DataElement Unqueued Valve r c,
        pressure :: DataElement Unqueued Valve r c }

type ValveP r = ValvePort r Closed

instance ComSpec (ValvePort Required) where
    type ComSpecFor (ValvePort Required) = InitValue (Bool,Bool)
    comSpec p (InitValue (rv,pv)) = do
        comSpec (relief p) (InitValue rv)
        comSpec (pressure p) (InitValue pv)

instance Port ValvePort where
        providedPort = do
            relief <- providedPort
            pressure <- providedPort
            return ValvePort{..}
        requiredPort = do
            relief <- requiredPort
            pressure <- requiredPort
            return ValvePort{..}
        connect a b = do
            connect (relief a) (relief b)
            connect (pressure a) (pressure b)
        delegateP ps = do
            relief <- delegateP [ v | ValvePort{ relief = v } <- ps ]
            pressure <- delegateP [ v | ValvePort{ pressure = v } <- ps ]
            return ValvePort{..}
        delegateR ps = do
            relief <- delegateR [ v | ValvePort{ relief = v } <- ps ]
            pressure <- delegateR [ v | ValvePort{ pressure = v } <- ps ]
            return ValvePort{..}

data WheelCtrl = WheelCtrl {
        valve :: ValveP Provided,
        accel :: DataElem Unqueued Accel Required,
        slip  :: DataElem Queued Slip Required }

wheel_ctrl :: Index -> AUTOSAR WheelCtrl
wheel_ctrl i = composition $ do
        ctrl <- controller
        (accel_p, ctrl_p, pressure) <- pressure_seq
        (accel_r, ctrl_r, relief) <- relief_seq
        connect  ctrl_p  (onoff_pressure ctrl)
        connect  ctrl_r  (onoff_relief ctrl)
        when (i==2) $ do
            probeWrite "relief 2"    relief
            probeWrite "pressure 2"  pressure
        accel <- delegate [accel_p, accel_r]
        let valve = ValvePort{..}
            slip = slipstream ctrl
        return $ WheelCtrl valve accel slip



--------------------------------------------------------------
-- The "main loop" of the ABS algorithm is a component that
-- periodically reads the current speeds of all wheels,
-- approximates the vehicle speed as the maximum of the wheel
-- speeds (should be refined for the case when all wheels are
-- locked, must resort do dead reckoning based on latest
-- de-acceleration in these cases), and sends every wheel
-- controller its updated slip ratio.
--------------------------------------------------------------

fromOk (Ok v) = v

slipRatio :: Double -> Double -> Double
slipRatio 0.0  _v = 1.0   -- no slip
slipRatio v0   v  = v/v0

data MainLoop = MainLoop {
        velos_in  :: [DataElem Unqueued Velo Required],
        slips_out :: [DataElem Queued   Slip Provided] }

main_loop :: AUTOSAR MainLoop
main_loop = atomic $ do
        velos_in <- replicateM 4 requiredPort
        slips_out <- replicateM 4 providedPort
        runnable (MinInterval 0) [TimingEvent 0.01] $ do
            velos <- mapM (liftM fromOk . rteRead) velos_in
            let v0 = maximum velos
            forM (velos `zip` slips_out) $ \(v,p) ->
                rteSend p (slipRatio v0 v)
        return $ sealBy MainLoop velos_in slips_out

--------------------------------------------------------------
-- A full ABS system consists of a main loop component as well
-- as one controller, one relief and one pressure component for
-- each wheel. In addition, each wheel must be equipped with
-- speed and acceleration sensors, but these parts are considered
-- external to the current system.
--------------------------------------------------------------

data WheelPorts = WheelPorts {
        velo_in   :: DataElem Unqueued Velo Required,
        accel_in  :: DataElem Unqueued Accel Required,
        valve_out :: ValveP Provided }

abs_system :: AUTOSAR [WheelPorts]
abs_system = composition $ do
        MainLoop velos_in slips_out <- main_loop
        w_ctrls <- forM [1..4] wheel_ctrl
        connectEach slips_out (map slip w_ctrls)
        let w_ports = zipWith3 WheelPorts velos_in (map accel w_ctrls) (map valve w_ctrls)
        return w_ports


-------------------------------------------------------------------
--  A simulated physical car
-------------------------------------------------------------------
wheel_f :: Index -> Time -> Bool -> Bool -> Velo -> (Accel, Velo)
wheel_f i time pressure relief velo
        | time < 1.0                = veloStep 0          velo
        | i /= 2                    = veloStep (-4.5)     velo
        -- let wheel 2 skid...
        -- We "postulate" a reasonable approximation of the wheel
        -- speed (ignoring the actual valves). Ideally the whole car
        -- dynamics whould be in another module (in Simulink).
        | time < 1.6                = veloStep (-10)      velo
        | time < 2                  = veloStep (-4)       velo
        | time < 2.5                = veloStep (-3)       velo
        | time < 3                  = veloStep 0.0        velo
        | time < 3.4                = veloStep (-4.5)     velo
        | time < 4                  = veloStep (-5)       velo
        | time < 4.3                = veloStep (-8.4)     velo
        | time < 4.7                = veloStep (-4)       velo
        | otherwise                 = veloStep 0          velo
-- The "pressure logic" is something like this, but a proper
-- treatment need integration over time and knowledge of vehicle speed
-- and other physical parameters.
--        | pressure && not relief    = veloStep (-10)      velo
--        | relief && not pressure    = veloStep (-1)       velo
--        | otherwise                 = veloStep (-3)       velo

timeStep :: Time
timeStep = 0.01

veloStep :: Accel -> Velo -> (Accel, Velo)
veloStep a v = (a, v + a*timeStep)

data Car = Car {
            actuators  :: [ValveP Required],
            v_sensors  :: [DataElem Unqueued Velo  Provided],
            a_sensors  :: [DataElem Unqueued Accel Provided] }

simulated_car :: AUTOSAR Car
simulated_car = atomic $ do
        wheels <- forM [1..4] $ \i -> do
            actuator <- requiredPort
            v_sensor <- providedPort
            a_sensor <- providedPort
            comSpec actuator (InitValue (False, True))
            comSpec v_sensor (InitValue init_v)
            comSpec a_sensor (InitValue init_a)
            when (i<=2) $ do
                probeWrite ("wheel "++show i++" speed")         v_sensor
                probeWrite ("wheel "++show i++" acceleration")  a_sensor
            return (actuator, v_sensor, a_sensor)
        irv <- interRunnableVariable (init_a, replicate 4 init_v)
        runnable (MinInterval 0) [TimingEvent timeStep] $ do
            Ok (time, velos) <- rteIrvRead irv
            let time' = time + timeStep
            velos' <- forM (zip3 [1..] wheels velos) $ \(i, (actuator,v_sensor,a_sensor), velo) -> do
                Ok pressure <- rteRead (pressure actuator) -- TODO: perhaps add error handling
                Ok relief   <- rteRead (relief actuator)   --   in case the return code is not "Ok v"
                let (acc, velo') = wheel_f i time' pressure relief velo
                rteWrite v_sensor velo'
                rteWrite a_sensor acc
                return velo'
            rteIrvWrite irv (time', velos')
            return ()
        let (actuators, v_sensors, a_sensors) = unzip3 wheels
        return $ sealBy Car actuators v_sensors a_sensors

init_v :: Velo
init_v = 18

init_a :: Accel
init_a = 0

-------------------------------------------------------------------------
-- A test setup consists of the ABS implementation and the simulated car
-- cyclically connected into a closed system.
--------------------------------------------------------------------------

test :: AUTOSAR ()
test = do
        w_ports <- abs_system
        car <- simulated_car
        connectEach (v_sensors car)         (map velo_in w_ports)
        connectEach (a_sensors car)         (map accel_in w_ports)
        connectEach (map valve_out w_ports) (actuators car)


makePlot :: Trace -> IO Bool
makePlot trace = plot (PDF "plot.pdf") curves
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

discrete :: Fractional t => [((q, t), k)] -> [(t, k)]
discrete []                     = []
discrete (((_,t),v):vs)         = (t,v) : disc v vs
  where disc v0 (((_,t),v):vs)  = (t,v0) : (t+eps,v) : disc v vs
        disc _ _                = []
        eps                     = 0.0001

scale :: (ProbeID, Measurement Bool) -> (ProbeID, Measurement Double)
scale ("relief 2", m)   = ("relief 2", map (fmap scaleValve) m)
  where scaleValve      = (+2.0) . fromIntegral . fromEnum
scale ("pressure 2", m) = ("pressure 2", map (fmap scaleValve) m)
  where scaleValve      = (+4.0) . fromIntegral . fromEnum

main1 :: IO Bool
main1 = simulateStandalone 5.0 output (RandomSched (mkStdGen 112)) test
  where output trace = printLogs trace >> makePlot trace



instance {-# OVERLAPS #-} ToExternal [WheelPorts] where
    toExternal ports    = toExternal (map valve_out ports)

instance ToExternal (ValvePort r c) where
  toExternal (ValvePort re pr) = toExternal re ++ toExternal pr

instance {-# OVERLAPS #-} FromExternal [WheelPorts] where
    fromExternal ports  = fromExternal (map velo_in ports) ++ fromExternal (map accel_in ports)


main2 = simulateUsingExternal abs_system


main = main1
