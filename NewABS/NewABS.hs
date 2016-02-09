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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import qualified Prelude
import Prelude ((++),map,even,replicate,round,odd,maximum,zip,zip3,zipWith3,fromIntegral,fromEnum)
import NewARSim
import Control.Monad
import Graphics.EasyPlot
import System.Random
import Feldspar

a && b = a ? b $ false
a || b = a ? true $ b

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
type Ticks = Data Int
type Limit = Data Int
type SeqIx = Data Int
--data SeqState = Stopped | Running Ticks Limit SeqIx deriving (Typeable,Data)
type SeqState = (Data Bool,Ticks,Limit,SeqIx)

stopped = (false, 0, 0, 0)
running ticks limit index = (true, ticks, limit, index)

sequencer :: Expr a =>
  RTE c SeqState -> (SeqIx -> RTE c SeqState) -> (a -> RTE c SeqState)
  -> AR c (ClientServerOperation a () Provided c)
sequencer setup step ctrl = do
        excl <- exclusiveArea
        state <- interRunnableVariable stopped
        runnable Concurrent [InitEvent] $ do
            rteEnter excl
            s <- setup
            rteIrvWrite state s
            rteExit excl
        runnable (MinInterval 0) [TimingEvent 0.001] $ do
            rteEnter excl
--            Ok s <- rteIrvRead state
            Ok s@(isRunning,ticks,limit,i) <- rteIrvRead state
            s' <- cond isRunning
                    (cond ((ticks+1) < limit)
                        (return (running (ticks+1) limit i))
                        (step i))
                    (return s)
--            s' <- case s of
--                    Stopped                    -> return s
--                    Running ticks limit i      -- Intended invariant: ticks < limit
--                            | ticks+1 < limit  -> return (running (ticks+1) limit i)
--                            | otherwise        -> step i
            rteIrvWrite state s'
            rteExit excl
        onoff <- provide ServerComSpec{bufferLength=0}
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
defaultSeqState = running 0 5 0 -- Count to five time steps (ms), then index 0

relief_setup :: DataElement Unqueued (Data Bool) Provided c -> RTE c SeqState
relief_setup valve = do
        rteWrite valve false
        return stopped

relief_step ::  DataElement Unqueued Valve Provided c ->
                DataElement Unqueued Accel Required c ->
                SeqIx -> RTE c SeqState
relief_step valve accel 0 = do
        Ok a <- rteRead accel
        printlog "" ("Relief " ++ show a)
        cond (a < 0)
            (do rteWrite valve true
                return (running 0 (round (-a*10)) 1))
            (return defaultSeqState)
relief_step valve accel n = do
        rteWrite valve false
        return defaultSeqState

relief_ctrl ::  DataElement Unqueued Valve Provided c   ->
                DataElement Unqueued Accel Required c   ->
                Valve -> RTE c SeqState
relief_ctrl valve accel v =
        cond v 
            (relief_step valve accel 0)
            (do rteWrite valve false
                return stopped)

newtype Relief c = Relief ( DataElement Unqueued Accel Required c,
                            ClientServerOperation (Data Bool) () Provided c,
                            DataElement Unqueued Valve Provided c )
    deriving Interface

--instance Interface Relief where
--    seal (Relief (a,c,v)) = Relief (seal a, seal c, seal v)

relief_seq :: AR c (Relief ())
relief_seq = atomic $ do
        valve <- provide UnqueuedSenderComSpec{initSend=Nothing}
        accel <- require UnqueuedReceiverComSpec{initValue=Nothing}
        ctrl <- sequencer (relief_setup valve)
                          (relief_step  valve accel)
                          (relief_ctrl  valve accel)
        return $ Relief (accel, ctrl, valve)

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
        rteWrite valve true
        return stopped

pressure_step ::
  DataElement Unqueued Valve Provided c  ->
  DataElement Unqueued Accel Required c  ->
  SeqIx -> RTE c SeqState
pressure_step valve accel 0 = do
        rteWrite valve true
        return (running 0 100 1)
pressure_step valve accel 20 = do
        rteWrite valve true
        return stopped
pressure_step valve accel n | even n = do
        rteWrite valve true
        Ok a <- rteRead accel
        printlog "" ("Pressure " ++ show a)
        return (running 0 (round (a*50)) (n+1))
pressure_step valve accel n | odd n = do
        rteWrite valve false
        return (running 0 20 (n+1))

pressure_ctrl ::
  DataElement Unqueued Valve Provided c  ->
  DataElement Unqueued Accel Required c  ->
  SeqIx -> RTE c SeqState
pressure_ctrl valve accel ix =
        cond (ix==2) 
            (do rteWrite valve true
                return stopped) $
        cond (ix==1)
            (pressure_step valve accel 0)
            (do rteWrite valve false
                return stopped)

newtype PresSeq c = PresSeq ( DataElement Unqueued Accel Required c,
                              ClientServerOperation SeqIx () Provided c,
                              DataElement Unqueued Valve Provided c)
    deriving Interface

--instance Interface PresSeq where
--    seal (PresSeq (a,c,v)) = PresSeq (seal a, seal c, seal v)

pressure_seq :: AR c (PresSeq ())
pressure_seq = atomic $ do
        valve <- provide UnqueuedSenderComSpec{initSend=Nothing}
        accel <- require UnqueuedReceiverComSpec{initValue=Nothing}
        ctrl <- sequencer (pressure_setup valve)
                          (pressure_step  valve accel)
                          (pressure_ctrl  valve accel)
        return $ PresSeq (accel, ctrl, valve)

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
data Controller c = Controller {
            slipstream     :: DataElement Queued Slip       Required c,
            onoff_pressure :: ClientServerOperation Int  () Required c,
            onoff_relief   :: ClientServerOperation Bool () Required c  }
    deriving Interface

--instance Interface Controller where
--    seal x = Controller {
--                    slipstream = seal (slipstream x),
--                    onoff_pressure = seal (onoff_pressure x),
--                    onoff_relief = seal (onoff_relief x)  }

controller :: AR c (Controller ())
controller = atomic $ do
        memo <- interRunnableVariable 1.0
        slipstream     <- require QueuedReceiverComSpec{queueLength=10}
        onoff_pressure <- require ClientComSpec
        onoff_relief   <- require ClientComSpec
        runnable (MinInterval 0) [DataReceivedEvent slipstream] $ do
            Ok slip   <- rteReceive slipstream
            Ok slip'  <- rteIrvRead memo
            cond (slip < 0.8 && slip' >= 0.8)
                    (do printlog "" ("Slip " ++ show slip)
                        rteCall onoff_pressure 0
                        rteCall onoff_relief true) $
                cond (slip >= 0.8 && slip' < 0.8)
                    (do printlog "" ("Slip " ++ show slip)
                        rteCall onoff_relief false
                        rteCall onoff_pressure (slip >= 1.0 ? 2 $ 1))
                    (return ())
            rteIrvWrite memo slip
        return $ Controller{..}

type Accel = Data Double
type Velo  = Data Double
type Valve = Data Bool

data ValvePort r c = ValvePort {
        relief   :: DataElement Unqueued Valve r c,
        pressure :: DataElement Unqueued Valve r c  }
    deriving Interface

--instance Interface (ValvePort r) where
--        seal x = ValvePort{ relief = seal (relief x), pressure = seal (pressure x) }

instance Port ValvePort where
        type PComSpec ValvePort = (UnqueuedSenderComSpec Valve, UnqueuedSenderComSpec Valve)
        type RComSpec ValvePort = (UnqueuedReceiverComSpec Valve, UnqueuedReceiverComSpec Valve)
        provide (a,b) = do
            relief <- provide a
            pressure <- provide b
            return ValvePort{..}
        require (a,b) = do
            relief <- require a
            pressure <- require b
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

data WheelCtrl c = WheelCtrl {
        accel :: DataElement Unqueued Accel Required c,
        slip  :: DataElement Queued Slip Required c,
        valve :: ValvePort Provided c  }
    deriving Interface

--instance Interface WheelCtrl where
--        seal x = WheelCtrl { accel = seal (accel x), valve = seal (valve x), slip = seal (slip x) }

wheel_ctrl :: SeqIx -> AR c (WheelCtrl ())
wheel_ctrl i = composition $ do
        ctrl <- controller
        PresSeq (accel_p, ctrl_p, pressure) <- pressure_seq
        Relief (accel_r, ctrl_r, relief) <- relief_seq
        connect  ctrl_p  (onoff_pressure ctrl)
        connect  ctrl_r  (onoff_relief ctrl)
        when (i==2) $ do
            probeWrite "relief 2"    relief
            probeWrite "pressure 2"  pressure
        accel <- delegate [accel_p, accel_r]
        let valve  = ValvePort{ .. }
            slip = slipstream ctrl
        return WheelCtrl{..}



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

data MainLoop c = MainLoop {
        velos_in  :: [DataElement Unqueued Velo Required c],
        slips_out :: [DataElement Queued   Slip Provided c] }

instance Interface MainLoop where
    seal ml = MainLoop {velos_in = map seal (velos_in ml), slips_out = map seal (slips_out ml)}

main_loop :: AR c (MainLoop ())
main_loop = atomic $ do
        velos_in <- replicateM 4 (require UnqueuedReceiverComSpec{initValue=Nothing})
        slips_out <- replicateM 4 (provide QueuedSenderComSpec)
        runnable (MinInterval 0) [TimingEvent 0.01] $ do
            velos <- mapM (liftM fromOk . rteRead) velos_in
            let v0 = maximum velos
            forM (velos `zip` slips_out) $ \(v,p) ->
                rteSend p (slipRatio v0 v)
        return MainLoop{..}

--------------------------------------------------------------
-- A full ABS system consists of a main loop component as well
-- as one controller, one relief and one pressure component for
-- each wheel. In addition, each wheel must be equipped with
-- speed and acceleration sensors, but these parts are considered
-- external to the current system.
--------------------------------------------------------------

data WheelPorts c = WheelPorts {
        velo_in   :: DataElement Unqueued Velo Required c,
        accel_in  :: DataElement Unqueued Accel Required c,
        valve_out :: ValvePort Provided c  }

abs_system :: AR c [WheelPorts ()]
abs_system = composition $ do
        MainLoop velos_in slips_out <- main_loop
        w_ctrls <- forM [1..4] wheel_ctrl
        connectEach slips_out (map slip w_ctrls)
        let w_ports = zipWith3 WheelPorts velos_in (map accel w_ctrls) (map valve w_ctrls)
        return (w_ports)


-------------------------------------------------------------------
--  A simulated physical car
-------------------------------------------------------------------
wheel_f :: SeqIx -> Time -> Bool -> Bool -> Velo -> (Accel, Velo)
wheel_f i time pressure relief velo
        | time < 1.0                = veloStep 0          velo
        | not (i == 2)              = veloStep (-4.5)     velo
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

data Wheel c = Wheel {
            actuator  :: ValvePort Required c,
            v_sensor  :: DataElement Unqueued Velo  Provided c,
            a_sensor  :: DataElement Unqueued Accel Provided c  }
    deriving Interface

--instance Interface Wheel where
--    seal wh = Wheel (seal (actuator wh)) (seal (v_sensor wh)) (seal (a_sensor wh))

newtype Car c = Car [Wheel c]

instance Interface Car where
    seal (Car wheels) = Car (map seal wheels)

car :: AR c (Car ())
car = atomic $ do
        wheels <- forM [1..4] $ \i -> do
            actuator <- require (UnqueuedReceiverComSpec{initValue=Just False},
                                 UnqueuedReceiverComSpec{initValue=Just True})
            v_sensor <- provide UnqueuedSenderComSpec{initSend=Just init_v}
            a_sensor <- provide UnqueuedSenderComSpec{initSend=Just init_a}
            when (i<=2) $ do
                probeWrite ("wheel "++show i++" speed")         v_sensor
                probeWrite ("wheel "++show i++" acceleration")  a_sensor
            return Wheel{..}
        irv <- interRunnableVariable (init_a, replicate 4 init_v)
        runnable (MinInterval 0) [TimingEvent timeStep] $ do
            Ok (time, velos) <- rteIrvRead irv
            let time' = time + timeStep
            velos' <- forM (zip3 [1..] wheels velos) $ \(i, Wheel{..}, velo) -> do
                Ok pressure <- rteRead (pressure actuator) -- TODO: perhaps add error handling
                Ok relief   <- rteRead (relief actuator)   --   in case the return code is not "Ok v"
                let (acc, velo') = wheel_f i time' pressure relief velo
                rteWrite v_sensor velo'
                rteWrite a_sensor acc
                return velo'
            rteIrvWrite irv (time', velos')
            return ()
        return (Car wheels)

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
        w_ports <- abs_system
        Car wheels <- car
        connectEach (map v_sensor wheels)   (map velo_in w_ports)
        connectEach (map a_sensor wheels)   (map accel_in w_ports)
        connectEach (map valve_out w_ports) (map actuator wheels)


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
main1 = printLogs trace >> makePlot trace
  where trace = limitTime 5.0 $ execSim (RandomSched (mkStdGen 111)) test


main :: IO Bool
main = main1
