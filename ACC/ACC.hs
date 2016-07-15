-- | Adaptive Cruise Control (ACC) component in the AUTOSAR monad.
--
-- The system includes a simulated target vehicle sensor, a PID controller for 
-- regulating vehicle speed, and an ACC component which composes all inputs and
-- sub-systems.
--
-- The system requires access to the brake and throttle controlling subsystems 
-- of the car, as well as /some/ input of the distance to the target vehicle. An
-- on/off switch is provided by reading an external double input (which is 
-- converted to a @Bool@ and regarded as @True@ whenever non-zero). The on/off 
-- switch essentially enables or bypasses the system.
--
-- *** TODO *** 
--
--  * External connections get @NO_DATA@? Can be fixed by comSpec:ing incoming
--    connections with InitValue, but that should not be necessary? Eliminated
--    if we use DataReceivedEvents for components where possible. 
--    XXX Scheduling? XXX
--
--  * Implement panic braking logic? Could re-use ABS state-machine from
--    NewABS2 (make vehicle state /global/.)
--

{-# LANGUAGE RecordWildCards #-}

module ACC 
  ( -- * ACC system
    ACCSystem(..)
  , accSystem
    -- * Types
  , Velo, Throttle
  ) where

import Control.Monad
import Generic
import NewARSim      hiding (void)
import PID

-- * Types 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Distance = Double
type Velo     = Double
type Throttle = Double

-- * ACC composition
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data ACCSystem = ACCSystem
  { -- Inputs
    accCruise   :: DataElem Unqueued Velo             Required
  , accVeloIn   :: DataElem Unqueued Velo             Required
  , accTarget   :: DataElem Unqueued (Maybe Distance) Required
    -- Outputs
  , accThrottle :: DataElem Unqueued Throttle         Provided
  , accBrake    :: DataElem Unqueued Throttle         Provided
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
     pidCtrl <- pdController (timeStep / 10) 5e-3 3e-2
     
     -- Connections
     connect (pidOp pidCtrl) (ctrl accECU)  
     
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
--
-- TODO: Read vehicle velocity from NewABS2 estimation algorithm.
--       Allow panic braking (apply NewABS2 state machine to target vehicle?)

-- | The ACC unit requires a target vehicle sensor, a set (cruise) speed, a 
-- controller of some sort for controlling vehicle speed, and access to brakes 
-- and throttle.
data CruiseCtrl = CruiseCtrl
  { -- Vehicle information
    crVel   :: DataElem Unqueued Velo               Required
  , vhVel   :: DataElem Unqueued Velo               Required
    -- Controller
  , ctrl    :: ClientServerOp (Velo, Velo) Throttle Required
    -- Target vehicle sensor
  , target  :: DataElem Unqueued (Maybe Velo)       Required
    -- Car subsystems
  , thrCtrl :: DataElem Unqueued Throttle           Provided
  , brkCtrl :: DataElem Unqueued Throttle           Provided
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
     -- TODO: * Target speed not relative to vehicle 
     --       * Override cruise speed
     --       * Implement some sort of panic braking rule 
     --         (use ABS state machine?)
     --
     runnable (MinInterval 0) [TimingEvent deltaT] $
       do Ok c <- rteRead crVel
          Ok v <- rteRead vhVel
         
          -- Try to override cruise speed when there is a target with a speed
          -- slower than cruise speed.
          --
          -- TODO: Chatter, speed is absolute, not relative, fix above.
          Ok rel <- rteRead target
          Ok sig <- case rel of 
                      Nothing -> rteCall ctrl (c, v)
                      Just r  -> rteCall ctrl (0, v)
         
          -- Read control signal and redirect depending of sign.
          if sig < 0 then
            do rteWrite thrCtrl 0
               rteWrite brkCtrl (-sig)
          else 
            do rteWrite thrCtrl sig
               rteWrite brkCtrl 0
                
     return $ sealBy CruiseCtrl crVel vhVel ctrl target thrCtrl brkCtrl

