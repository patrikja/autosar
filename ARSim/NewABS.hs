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
  -> AR c (ProvidedOperation a () ())
sequencer setup step ctrl = do
        excl <- exclusiveArea
        state <- interRunnableVariable Stopped
        runnable Concurrent [Init]              (seq_init setup excl state)
        runnable (MinInterval 0) [Timed 0.001]  (seq_tick step  excl state)
        onoff <- providedOperation
        serverRunnable (MinInterval 0) [onoff]  (seq_onoff ctrl excl state)
        return (seal onoff)

--------------------------------------------------------------
-- A relief component is a sequencer for toggling a brake relief valve
-- in an ABS system. It requires a data element for reading wheel
-- acceleration values (of type Accel = Double) and pulses the valve
-- in lengths proportional to acceleration, as long as this value
-- stays negative. It provides a boolean on/off control operation as
-- well as a boolean data element producing valve settings.
--------------------------------------------------------------

relief_setup :: ProvidedDataElement Bool c -> RTE c SeqState
relief_setup valve = do
        rteWrite valve False
        return Stopped
        
relief_step ::  ProvidedDataElement  Valve  c -> 
                RequiredDataElement  Accel  c -> 
                Index -> RTE c SeqState
relief_step valve accel 0 = do
        Ok a <- rteRead accel
        printlog "A" ("Relief " ++ show a)
        if a < 0 then do
                rteWrite valve True
                return (Running 0 (round (-a*10)) 1)
         else
                return (Running 0 5 0)
relief_step valve accel n = do
        rteWrite valve False
        return (Running 0 5 0)

relief_ctrl ::  ProvidedDataElement  Valve  c   -> 
                RequiredDataElement  Accel  c   -> 
                Valve -> RTE c SeqState
relief_ctrl valve accel True = 
        relief_step valve accel 0
relief_ctrl valve accel False = do
        rteWrite valve False
        return Stopped
        
type Relief = ( RequiredDataElem Accel, 
                ProvidedOp Bool (), 
                ProvidedDataElem Valve)
relief_seq :: AR c Relief
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

type PresSeq = ( RequiredDataElem Accel, 
                 ProvidedOp Index (), 
                 ProvidedDataElem Valve)

pressure_setup :: ProvidedDataElement Valve c -> RTE c SeqState
pressure_setup valve = do
        rteWrite valve True
        return Stopped

pressure_step :: 
  ProvidedDataElement  Valve  c  -> 
  RequiredDataElement  Accel  c  -> 
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
        printlog "A" ("Pressure " ++ show a)
        return (Running 0 (round (a*50)) (n+1))
pressure_step valve accel n | odd n = do
        rteWrite valve False
        return (Running 0 20 (n+1))

pressure_ctrl :: 
  ProvidedDataElement  Valve  c  -> 
  RequiredDataElement  Accel  c  -> 
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

control ::
  (Data pres, Num pres,
   Data r1, 
   Data q1) =>
  InterRunnableVariable   Slip  c  -> 
  RequiredOperation pres  q1    c  -> 
  RequiredOperation Bool  r1    c  -> 
  RequiredQueueElement    Slip  c  -> 
  RTE c (StdRet ())
control memo onoff_pressure onoff_relief slipstream = do
        Ok slip   <- rteReceive slipstream
        Ok slip'  <- rteIrvRead memo
        case (slip < 0.8, slip' < 0.8) of
                (True, False) -> do
                        printlog "A" ("Slip " ++ show slip)
                        rteCall onoff_pressure 0
                        rteCall onoff_relief True
                        return ()
                (False, True) -> do
                        printlog "A" ("Slip " ++ show slip)
                        rteCall onoff_relief False
                        rteCall onoff_pressure (if slip >= 1.0 then 2 else 1)
                        return ()
                _ ->    return ()
        rteIrvWrite memo slip

type Slip = Double
type ABSstate = (RequiredQueueElem Slip, RequiredOp Int (), RequiredOp Bool ())
controller :: AR c ABSstate
controller = component $ do
        memo <- interRunnableVariable 1.0
        onoff_pressure <- requiredOperation
        onoff_relief <- requiredOperation
        slipstream <- requiredQueueElement 10
        runnable (MinInterval 0) [ReceiveQ slipstream] 
                (control memo onoff_pressure onoff_relief slipstream)
        return (seal slipstream, seal onoff_pressure, seal onoff_relief)

type Accel = Double
type Velo  = Double
type Valve = Bool

wheel_ctrl :: (Index, ProvidedQueueElement Slip ()) -> AR c WheelCtrl
wheel_ctrl (i,slipstream) = component $ do
        (slip, onoff_pressure, onoff_relief) <- controller
        (accel_p, ctrl_p, valve_p) <- pressure_seq
        (accel_r, ctrl_r, valve_r) <- relief_seq
        connect slipstream slip
        connect onoff_pressure ctrl_p
        connect onoff_relief ctrl_r
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

loop :: [RequiredDataElement   Velo  c] -> 
        [ProvidedQueueElement  Velo  c] ->
        RTE c [StdRet ()]
loop velostreams slipstreams = do
        velos <- mapM (\re -> do Ok v <- rteRead re; return v) velostreams
        let v0 = maximum velos
        mapM (\(v,pe) -> rteSend pe (slip v0 v)) (velos `zip` slipstreams)

slip :: Double -> Double -> Double
slip 0.0  _v = 1.0   -- no slip
slip v0   v  = v/v0

main_loop :: AR c ([RequiredDataElem Velo], [ProvidedQueueElem Slip])
main_loop = component $ do
        velostreams <- replicateM 4 requiredDataElement
        slipstreams <- replicateM 4 providedQueueElement
        runnable (MinInterval 0) [Timed 0.01] (loop velostreams slipstreams)
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


type Wheel' c = (RequiredDataElement Valve  c,
                 RequiredDataElement Valve  c,
                 ProvidedDataElement Velo   c,
                 ProvidedDataElement Accel  c)

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
        r_act  <- requiredDataElementInit False
        p_act  <- requiredDataElementInit True
        v_sens <- providedDataElementInit init_v
        a_sens <- providedDataElementInit init_a
        when (i<=2) $ do
            probeWrite ("wheel "++show i++" speed")         v_sens
            probeWrite ("wheel "++show i++" acceleration")  a_sens
        return (r_act, p_act, v_sens, a_sens)

car :: AR c [Wheel]
car = do
        wheels <- mapM mk_wheel [1..4]
        irv <- interRunnableVariable (init_a, replicate 4 init_v)
        runnable (MinInterval 0) [Timed 0.01] (simul wheels irv)
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

type VelocityIn = RequiredDataElem Velo
type WheelCtrl = (  RequiredDataElem Accel,
                    RequiredDataElem Accel,
                    ProvidedDataElem Valve,
                    ProvidedDataElem Valve)

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
        discrete []                     = []
        discrete (((_,t),v):vs)         = (t,v) : disc v vs
          where disc v0 (((_,t),v):vs)  = (t,v0) : (t+eps,v) : disc v vs
                disc _ _                = []
                eps                     = 0.0001
        ms                              = ("relief 2",bools1):("pressure 2",bools2):doubles
        doubles                         = probeAll trace
        bools1                          = map (fmap scaleValve_r) (probe trace "relief 2")
        bools2                          = map (fmap scaleValve_p) (probe trace "pressure 2")

scaleValve_r :: Bool -> Double
scaleValve_r = (+2.0) . fromIntegral . fromEnum
scaleValve_p :: Bool -> Double
scaleValve_p = (+4.0) . fromIntegral . fromEnum

main1 :: IO Bool
main1 = printLogs trace >> makePlot trace
  where trace = limitTime 5.0 $ execSim (RandomSched (mkStdGen 111)) test


main :: IO Bool
main = main1

{-
rmethod rport excl irv = do
        Ok v <- rteRead rport
        rteEnter excl
        Ok s <- rteIrvRead irv
        rteIrvWrite irv (s+v)
        rteExit excl

wmethod pport peek = do
        x <- peek
        rteWrite pport (7+x)

peek irv = do
        Ok v <- rteIrvRead irv
        return v

tricky = do (rp,peek)   <- component $ do rport <- requiredDataElement
                                          excl <- exclusiveArea
                                          irv <- interRunnableVariable (0::Int)
                                          runnable Concurrent [ReceiveE rport] (rmethod rport excl irv)
                                          return (seal rport, peek irv)
            pp          <- component $ do pport <- providedDataElement
                                          runnable Concurrent [Timed 1.0] (wmethod pport peek)
                                          return (seal pport)
            connect pp rp
-}
