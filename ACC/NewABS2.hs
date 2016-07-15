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

module NewABS2
  ( -- * ABS system
    ABS(..)
  , absSystem
    -- * Valve ports
  , ValvePort(..)  , ValveP
    -- * Wheel ports
  , WheelPorts(..)
  ) where

import Control.Monad
import Data.List
import NewARSim
import PID
import System.Random

-- * Types
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Slip  = Double
type Accel = Double
type Velo  = Double
type Valve = Bool
type Index = Int

-- * Bang-bang controller
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data BangBang = BangBang
  { valves  :: ValveP                             Provided
  , slips   :: DataElem Unqueued Slip             Required 
  , pidCall :: ClientServerOp (Slip, Slip) Double Required
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
     runnable (MinInterval 0) [TimingEvent 1e-3] $
       do Ok c  <- rteIrvRead carrier
          Ok s0 <- rteRead slips
          Ok s  <- rteCall pidCall (0.2, s0)

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
     pid  <- pidController 1e-4 2e-3 1e324 7

     connect (pidOp pid) (pidCall bang)
     
     valve <- providedDelegate [valves bang]
     slip  <- requiredDelegate [slips bang]

     when (i == 2) $
       do probeWrite "pressure 2" (pressure (valves bang))
          probeWrite "relief 2"   (relief   (valves bang))

     return $ WheelCtrl valve slip i

-- * Main loop
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Uses the velocity and slip estimation algorithms and reports slip estimates
-- to the ABS controller. 

-- | The main loop component requires a longitudal accelerometer (which is
-- external to the current setup), as well as wheel velocity measurements (also
-- assumed external), and provides wheel slip estimates.
data MainLoop = MainLoop 
  { velosIn  :: [DataElem Unqueued Velo Required]
    -- ^ Wheel velocities
  , slipsOut :: [DataElem Unqueued Slip Provided] 
    -- ^ Wheel slips
  , velo     :: DataElem Unqueued Velo Required
    -- ^ Vehicle velocity estimate
  }

-- | The main loop of the ABS algorithm approximates wheel slip ratios and sends
-- these to the PWM.
mainLoop :: AUTOSAR MainLoop
mainLoop = atomic $ 
  do velosIn  <- replicateM 4 requiredPort
     slipsOut <- replicateM 4 providedPort
     velo     <- requiredPort
      
     mapM_ (`comSpec` InitValue 0.0) slipsOut
      
     runnable (MinInterval 0) [DataReceivedEvent velo] $
       do velos <- mapM (fmap fromOk . rteRead) velosIn
          Ok v1 <- rteRead velo
          forM (velos `zip` slipsOut) $ \(v, p) -> rteWrite p (slipRatio v1 v)

     return $ sealBy MainLoop velosIn slipsOut velo

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

data ABS = ABS 
  { absVeloIn  :: DataElem Unqueued Double Required
    -- ^ Velocity estimate
  , wheelPorts :: [WheelPorts]
    -- ^ Wheelports
  }

-- | The ABS system connects the @mainLoop@ component for each wheel with a
-- wheel controller. Each controller is also connected to the carrier wave
-- sequencer.
absSystem :: AUTOSAR ABS
absSystem = composition $ 
  do -- Main loop
     MainLoop velosIn slipsOut veloIn <- mainLoop
     
     -- Wheel controllers
     wheelCtrls <- forM [1..4] wheelCtrl

     -- Connect slips to wheel controllers
     connectEach slipsOut (map slip wheelCtrls)

     let vs = map valve wheelCtrls
         ss = map slip  wheelCtrls
         is = map index wheelCtrls
     return $ ABS veloIn (zipWith3 WheelPorts velosIn vs is)

instance External WheelPorts where
  toExternal   (WheelPorts _ r idx) = addNum idx $ toExternal r 
  fromExternal (WheelPorts v _ idx) = 
    addNum idx $ relabel "VELO" (fromExternal v) 

instance External (ValvePort r c) where
  toExternal (ValvePort re pr) = 
    relabel "RE" (toExternal re) ++ 
    relabel "PR" (toExternal pr)

