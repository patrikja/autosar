-- | Adaptive Cruise Control. 

{-# LANGUAGE RecordWildCards   #-}

module ACC 
  ( -- * ACC system
    ACCSystem(..)
  , accSystem
    -- * Types
  , Velo, Throttle
  ) where

import Control.Monad
import Data.Maybe
import Generic
import Revlimit 
import NewARSim
import PID

-- * Types 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Dist     = Double
type Velo     = Double
type Throttle = Double

-- * ACC composition
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data ACCSystem = ACCSystem
  { -- Inputs
    accCruise   :: DataElem Unqueued Velo Required
  , accVeloIn   :: DataElem Unqueued Velo Required
  , accTarget   :: DataElem Unqueued (Maybe (Velo, Dist)) Required
    -- Outputs
  , accThrottle :: DataElem Unqueued Throttle Provided
  , accBrake    :: DataElem Unqueued Throttle Provided
  }

-- | Adaptive Cruise Control system.
accSystem :: Time -> AUTOSAR ACCSystem
accSystem timeStep = composition $
  do -- Sample time resolution of the entire system. Inherited 
     -- by the target vehicle sensor and the ACC ECU.
    
     accECU <- cruiseCtrl timeStep

     -- PD controller
     -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     -- Create a PD controller for the ACC. Parameters are 
     -- found by trial-and-error.
     pidCtrl <- pdController (timeStep / 10) 5e-3 5e-2

     -- Connections
     connect (pidOp pidCtrl)    (ctrl accECU)
     
     accCruise <- requiredDelegate [crVel accECU]
     accVeloIn <- requiredDelegate [vhVel accECU]
     accTarget <- requiredDelegate [target accECU]

     accThrottle <- providedDelegate [thrCtrl accECU]
     accBrake    <- providedDelegate [brkCtrl accECU]
     
     return ACCSystem {..}

-- * Adaptive Cruise Control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The adaptive cruise-control consists of a PD controller for achieving the
-- desired cruise velocity during operations, a radar component input which is 
-- assumed to provide the relative speed of some large enough object ahead of 
-- the vehicle. 

-- | The ACC unit requires a target vehicle sensor, a set (cruise) speed, a 
-- controller of some sort for controlling vehicle speed, and access to brakes 
-- and throttle.
data CruiseCtrl = CruiseCtrl
  { -- Vehicle information
    crVel   :: DataElem Unqueued Velo Required
  , vhVel   :: DataElem Unqueued Velo Required
    -- Controller
  , ctrl    :: ClientServerOp (Velo, Velo) Throttle Required
    -- Target vehicle sensor
  , target  :: DataElem Unqueued (Maybe (Velo, Dist)) Required
    -- Car subsystems
  , thrCtrl :: DataElem Unqueued Throttle Provided
  , brkCtrl :: DataElem Unqueued Throttle Provided
  }

-- | The cruise control apparatus.
cruiseCtrl :: Time 
           -- ^ Sample time
           -> AUTOSAR CruiseCtrl
cruiseCtrl deltaT = atomic $ 
  do crVel   <- requiredPort
     vhVel   <- requiredPort
     target  <- requiredPort
     ctrl    <- requiredPort

     brkCtrl <- providedPort
     thrCtrl <- providedPort
     comSpec brkCtrl (InitValue 0.0)
     comSpec thrCtrl (InitValue 0.0)
     
     comSpec crVel  (InitValue 0.0)
     comSpec vhVel  (InitValue 0.0)
     comSpec target (InitValue Nothing)

     -- Cruise control mode.
     -- ~~~~~~~~~~~~~~~~~~~~
--      runnable (MinInterval 0) [TimingEvent deltaT] $
     runnableT ["core1" :>> (3, 10)] (MinInterval 0) [TimingEvent deltaT] $ 
       do Ok c <- rteRead crVel
          Ok v <- rteRead vhVel
          Ok m <- rteRead target
          let (vt, st) = fromMaybe (c, 100) m

          Ok sig <- if v > vt then 
                      rteCall ctrl (0.9 * vt, v)
                    else 
                      rteCall ctrl (min c vt, v)

          if sig < 0 then do
            rteWrite thrCtrl 0
            rteWrite brkCtrl $ if st + vt - v <= 1 then (-sig) else (-sig/2)
          else do
            rteWrite thrCtrl sig
            rteWrite brkCtrl 0

     return $ sealBy CruiseCtrl crVel vhVel ctrl target thrCtrl brkCtrl

