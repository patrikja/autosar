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

{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module NewABS2
  ( -- * ABS system
    absSystem
    -- * Valve ports
  , ValvePort(..)  , ValveP
    -- * Wheel ports
  , WheelPorts(..)
    -- * Misc. types
  , Velo, Accel
  ) where

import Control.Monad
import Data.List
import NewARSim
import System.Random

-- * Types
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Slip  = Double
type Accel = Double
type Velo  = Double
type Valve = Bool
type Index = Int

-- * PID controller
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | The PID controller requires a @(cruise, vehicle)@ velocity tuple and
-- provides a throttle control.
newtype PIDCtrl = PIDCtrl
  { pidOp :: ClientServerOp (Double, Double) Double Provided }

-- | The PID controller produces a linear combination of Propotional-, 
-- Integrating and Differentiating gain. The component requires the previous
-- state of the variables and is thus stateful.
--
-- NOTE: Derivatives get harsh over time, should perhaps filter inputs?
pidController :: Time 
              -- ^ Sample time
              -> Time
              -- ^ Derivative time 
              -> Time
              -- ^ Integral time
              -> Double
              -- ^ Proportional scale 
              -> AUTOSAR PIDCtrl
              -- ^ Throttle control (output)
pidController dt td ti k = atomic $ do
     state <- interRunnableVariable (0.0, 0.0)
     pidOp <- providedPort
     serverRunnable (MinInterval 0) [OperationInvokedEvent pidOp] $
       \(ctrl, feedback) -> do
          Ok (prevErr, prevInt) <- rteIrvRead state
         
          let err        = ctrl - feedback
              derivative = (err - prevErr) / dt
              integral   = prevInt + dt / ti * err
              output     = k * (err + integral + td * derivative) 
          rteIrvWrite state (err, integral) 
          return output
     return $ sealBy PIDCtrl pidOp

-- * Bang-bang controller
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data BangBang = BangBang
  { valves  :: ValveP                                 Provided
  , slips   :: DataElem Unqueued Slip                 Required 
  , pidCall :: ClientServerOp (Double, Double) Double Required
  }

-- | Bang-bang controller using pulse width modulation control. Makes use of 
-- a PD-controller to control input signal, and pulse-width modulates the
-- control signal.
bangBangCtrl :: AUTOSAR BangBang
bangBangCtrl = atomic $ 
  do valvePort <- providedPort
     slips     <- requiredPort 
     pidCall   <- requiredPort
     comSpec valvePort (InitValue (False, True))

     carrier <- interRunnableVariable (0 :: Int)
  
     let width = 20
     -- Run on a 1 ms resolution
     runnable (MinInterval 0) [TimingEvent 0.001] $
       do Ok c  <- rteIrvRead carrier
          Ok s0 <- rteRead slips
          Ok s <- rteCall pidCall (0.2, s0)

          -- Control signal modulation
          -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          -- Cut-off to @[-1, 1]@. If width * |u0| exceeds carrier 
          -- wave, the signal is non-zero, otherwise zero. Negative
          -- control signals are re-routed to the relief actuators,
          -- positive control signals are re-routed to the pressure
          -- actuators.
          let u0     = (min 1 . max (-1)) s 
              scaled = truncate (abs u0 * fromIntegral width)

          if u0 < 0 then 
            do rteWrite (relief valvePort)   (scaled > c)
               rteWrite (pressure valvePort) False
          else 
            do rteWrite (relief   valvePort) False
               rteWrite (pressure valvePort) ((scaled > c) || s < 0.01)

          rteIrvWrite carrier ((c + 1) `mod` width)

     return $ sealBy BangBang valvePort slips pidCall

-- * Valve ports
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | ABS controller valve ports.
data ValvePort r c = ValvePort 
  { relief   :: DataElement Unqueued Valve r c
  , pressure :: DataElement Unqueued Valve r c
  }

type ValveP r = ValvePort r Closed

instance ComSpec (ValvePort Provided) where
  type ComSpecFor (ValvePort Provided) = InitValue (Bool, Bool)
  comSpec p (InitValue (rv, pv)) =
    do comSpec (relief p)   (InitValue rv)
       comSpec (pressure p) (InitValue pv)

instance ComSpec (ValvePort Required) where
  type ComSpecFor (ValvePort Required) = InitValue (Bool, Bool)
  comSpec p (InitValue (rv, pv)) = 
    do comSpec (relief p)   (InitValue rv)
       comSpec (pressure p) (InitValue pv)

instance Port ValvePort where
  providedPort =
    do relief   <- providedPort
       pressure <- providedPort
       return ValvePort {..}
  requiredPort = 
    do relief   <- requiredPort
       pressure <- requiredPort
       return ValvePort {..}
  connect a b = 
    do connect (relief a)   (relief b)
       connect (pressure a) (pressure b)
  providedDelegate ps = 
    do relief   <- providedDelegate [ v | ValvePort{ relief = v } <- ps ]
       pressure <- providedDelegate [ v | ValvePort{ pressure = v } <- ps ]
       return ValvePort {..}
  requiredDelegate ps = 
    do relief   <- requiredDelegate [ v | ValvePort{ relief = v } <- ps ]
       pressure <- requiredDelegate [ v | ValvePort{ pressure = v } <- ps ]
       return ValvePort {..}

-- * Wheel control module
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | ABS wheel control module.
data WheelCtrl = WheelCtrl 
  { valve   :: ValveP                 Provided
  , slip    :: DataElem Unqueued Slip Required
  , index   :: Int
  }

-- | Wheel control module. Connects the pressure valves to the ABS controller
-- and the carrier wave unit. Better results are achieved when the PID uses a
-- time step below the rest of the system. 
wheelCtrl :: Index -> AUTOSAR WheelCtrl
wheelCtrl i = composition $ 
  do bang <- bangBangCtrl
     pid  <- pidController 1e-4 1e-2 (1/0) 6
     
     connect (pidOp pid) (pidCall bang)

     valve <- providedDelegate [valves bang]
     slip  <- requiredDelegate [slips bang]

     when (i == 2) $
       do probeWrite "pressure 2" (pressure (valves bang))
          probeWrite "relief 2"   (relief   (valves bang))

     return $ WheelCtrl valve slip i

-- * Main loop
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Performs vehicle velocity and slip estimation.

-- | The main loop component requires a longitudal accelerometer (which is
-- external to the current setup), as well as wheel velocity measurements (also
-- assumed external).
data MainLoop = MainLoop 
  { velosIn  :: [DataElem Unqueued Velo Required]
    -- ^ Wheel velocities
  , slipsOut :: [DataElem Unqueued Slip Provided] 
    -- ^ Wheel slips
  , accelIn  :: DataElem Unqueued Double Required
    -- ^ Longtitudal accelerometer
  }

-- | The main loop of the ABS algorithm approximates wheel slip ratios and sends
-- these to the PWM.
--
-- The algorithm used assumes that the first two wheel velocity inputs come from
-- the non-driving wheels of the vehicle, and that a longtitudal accelerometer
-- is mounted on the unit. The slip calculation algorithm is borrowed from 
-- Active Braking Control Systems: Design for Vehicles by Savaresi and Tanelli,
-- chapter 5.
mainLoop :: AUTOSAR MainLoop
mainLoop = atomic $ 
  do velosIn  <- replicateM 4 requiredPort
     slipsOut <- replicateM 4 providedPort
     accelIn  <- requiredPort 
      
     mapM_ (`comSpec` InitValue 0.0) slipsOut
     mapM_ (`comSpec` InitValue 0.0) velosIn
     comSpec accelIn (InitValue 0.0)
      
     let deltaT = 1e-3
         memory = 100
    
     -- Keep acceleration memory for backwards integration phase.
     accMem  <- interRunnableVariable (replicate memory 0.0)
     avgMem  <- interRunnableVariable (replicate memory 0.0)
     veloMem <- interRunnableVariable 0.0 
     state   <- interRunnableVariable S0

     runnableT ["abs_mainloop_task" :-> 0] 
               (MinInterval 0) 
               [TimingEvent deltaT] $ 
       do velos  <- mapM (fmap fromOk . rteRead) velosIn
          Ok acc <- rteRead accelIn
          Ok s0  <- rteIrvRead state
          Ok v0  <- rteIrvRead veloMem
          Ok as  <- rteIrvRead accMem
          Ok avs <- rteIrvRead avgMem
           
          -- Velocity estimation
          -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          -- Assume that accelerometer input is filtered. This could be 
          -- done here, provided that we implement a FIR filter.
          --
          -- For low speeds or states with very little acceleration,
          -- approximate the vehicle speed as the average of the wheel
          -- speeds. For high acceleration, use the average of the non-
          -- driving wheels. For hard braking, use the back integration
          -- algorithm with the on-board accelerometer.
          let vAvg   = sum velos / fromIntegral (length velos) 
              nonDr  = take 2 velos
              vNd    = sum nonDr / fromIntegral (length nonDr) 

              -- Get next state
              s1 = stateTrans vAvg acc s0

              -- Compute vehicle velocity estimate.
              -- If the state transition was from S0 to S1, we've gone from
              -- soft to hard deceleration and should perform backwards 
              -- integration for @memory@ steps. If the state transition was
              -- from S1 to SM2, we ignore it since we're reaching a full stop
              -- doing hard braking.
              v1 = case s1 of
                     SM2 | s0 /= S1  -> vAvg
                         | otherwise -> v0 + acc * deltaT 
                     SM1             -> vNd
                     S0              -> vAvg
                     S1  | s0 == S1  -> v0  + acc * deltaT
                         | otherwise -> v0' + acc * deltaT
                         where
                           v0' = foldl (\v a -> v + a * deltaT) 
                                       (last avs) 
                                       (reverse as)

          -- Write memo-variables
          rteIrvWrite accMem  (acc:init as)
          rteIrvWrite veloMem v1
          rteIrvWrite avgMem  (vAvg:init avs)

          rteIrvWrite state s1

          forM (velos `zip` slipsOut) $ \(v, p) -> rteWrite p (slipRatio v1 v)

     return $ sealBy MainLoop velosIn slipsOut accelIn

-- * Slip estimation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- To generate a good estimate of wheel slip when all wheels are locked (or
-- nearing lock-up at the same speed), velocity estimation is done by
-- integrating the vehicle acceleration. To simplify this, the vehicle is kept
-- in one of four states, where
--
--  SM2  ->  Vehicle is moving at low speed
--  SM1  ->  Vehicle is accelerating
--  S0   ->  Vehicle is moving at constant speed or braking very lightly
--  S1   ->  Vehicle is braking hard
--
--  Velocity estimations will differ depending on the state which the vehicle
--  is in.

-- | Vehicle state machine.
data CState = SM2 | SM1 | S0 | S1
  deriving (Data, Eq, Typeable)

-- | Vehicle state machine transitions.
stateTrans :: Double -- ^ Average velocity
           -> Double -- ^ Longtitudal acceleration
           -> CState -- ^ Input state
           -> CState
stateTrans v a state =
  case state of
    SM2 | v <= vMin + hv  -> SM2
        | a >= delta      -> SM1
        | a >= -beta      -> S0
        | otherwise       -> S1
    SM1 | v <= vMin       -> SM2
        | a >= delta      -> SM1
        | a >= -beta      -> S0
        | otherwise       -> S1
    S0  | v <= vMin       -> SM2
        | a >= delta      -> SM1
        | a >= -beta      -> S0
        | otherwise       -> S1
    S1  | v <= vMin       -> SM2
        | a >= delta      -> SM1
        | a >= -beta + ha -> S0
        | otherwise       -> S1
  where
    vMin   = 0.5              -- Low velocity threshold
    hv     = 0.2              -- Velocity hysteresis
    ha     = 0.1              -- Acceleration hysteresis
    beta   = 0.8              -- Deceleration threshold
    delta  = 0.1              -- Acceleration threshold


fromOk (Ok v) = v

-- | @'slipRatio' v0 v@ calculates wheel slip based on vehicle velocity @v0@ and
-- wheel (tangential) velocity @v@. Slip ranges from 0 (no slip) to 1 (full
-- slip).
slipRatio :: Double -> Double -> Double
slipRatio 0  _ = 0
slipRatio v0 v = 1 - v/v0

-- * ABS system
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A full ABS system consists of a main loop component as well as one 
-- controller, one relief and one pressure component for each wheel. In
-- addition, each wheel must be equipped with speed and acceleration sensors,
-- but these parts are considered external to the current system.

-- Keeps an index for labeling, and approximated slips for reference.
data WheelPorts = WheelPorts 
  { veloIn   :: DataElem Unqueued Velo Required
  , valveOut :: ValveP                 Provided
  , wportIdx :: Int
  }

newtype ABS = ABS (DataElem Unqueued Double Required, [WheelPorts])

-- | The ABS system connects the @mainLoop@ component for each wheel with a
-- wheel controller. Each controller is also connected to the carrier wave
-- sequencer.
absSystem :: AUTOSAR ABS
absSystem = composition $ 
  do -- Main loop
     MainLoop velosIn slipsOut accelIn <- mainLoop
     
     -- Wheel controllers
     wheelCtrls <- forM [1..4] wheelCtrl

     -- Connect slips to wheel controllers
     connectEach slipsOut (map slip wheelCtrls)

     let vs = map valve wheelCtrls
         ss = map slip  wheelCtrls
         is = map index wheelCtrls

     declareTask "abs_mainloop_task" (TimingEvent 1e-3)

     return $ ABS (accelIn, zipWith3 WheelPorts velosIn vs is)

instance External ABS where
  toExternal   (ABS (_, ws)) = toExternal ws
  fromExternal (ABS (a, ws)) = 
    relabel "ACCEL" (toExternal a) ++ fromExternal ws

instance External WheelPorts where
  toExternal   (WheelPorts _ r idx) = addNum idx $ toExternal r 
  fromExternal (WheelPorts v _ idx) = 
    addNum idx $ relabel "VELO" (fromExternal v) 

instance External (ValvePort r c) where
  toExternal (ValvePort re pr) = 
    relabel "RE" (toExternal re) ++ 
    relabel "PR" (toExternal pr)

