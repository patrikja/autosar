{-# LANGUAGE RecordWildCards #-}

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
--  * Override cruise speed when target vehicle has a lower relative speed.
--  * Let radar ignore targets exceeding some threshold distance.
--  * Implement some sort of panic brake logic that goes into play when
--    cruise speed is overridden. When changing cruise speeds, we should refrain
--    from using brakes as much as possible.
--  * External connections get @NO_DATA@ early on. This is not supposed to
--    happen (there should always be a ZERO input at the very least).
--
module ACC 
  ( -- * Cruise control
    CruiseCtrl(..)
  , cruiseCtrl
    -- * Vehicle IO module
  , IOModule(..)
  , vehicleIO
    -- * Types
  , Velo, Throttle, Distance
    -- * PID controller
  , PIDCtrl(..)
  , pidController
  ) where

import Control.Monad
import Generic
import Gearbox
import NewARSim      hiding (void)
import Target

-- * Types 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- These are essentially unitless (in the sense that it does not matter), so
-- long as all values come from sources of the same unit. For simplicity, use SI
-- units (i.e. m/s for velocity, rad/s for angular velocity, etc, etc).

-- | (Vehicle) velocity.
type Velo = Double

-- | Throttle application. Unitless/fraction.
type Throttle = Double

-- | Brake pedal application. Unitless/fraction.
type Pedal = Double

-- * PID controller
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | The PID controller requires a @(cruise, vehicle)@ velocity tuple and
-- provides a throttle control.
data PIDCtrl = PIDCtrl
  { pidInput  :: DataElem Unqueued (Velo, Velo) Required
  , pidOutput :: DataElem Unqueued Throttle     Provided
  }

-- | The PID controller produces a linear combination of Propotional-, 
-- Integrating and Differentiating gain. The component requires the previous
-- state of the variables and is thus stateful.
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

     pidInput  <- requiredPort
     pidOutput <- providedPort 
     comSpec pidOutput (InitValue 0.0)

     probeWrite "PID OUT" pidOutput

     runnable (MinInterval 0) [DataReceivedEvent pidInput] $
       do Ok (ctrl, feedback)   <- rteRead pidInput
          Ok (prevErr, prevInt) <- rteIrvRead state
         
          let err        = ctrl - feedback
              derivative = (err - prevErr) / dt
              integral   = prevInt + dt / ti * err
              output     = k * (err + integral + td * derivative) 
          rteIrvWrite state (err, integral) 
          rteWrite pidOutput output
     return $ sealBy PIDCtrl pidInput pidOutput

-- * Target vehicle sensor
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A basic simulated radar. The system is intended to simulate the distance to
-- some moving object ahead of the vehicle, and report both relative speed as
-- well as distance to the object ahead. 

-- | Radar controller exports.
data RadarCtrl = RadarCtrl
  { distance :: DataElem Unqueued Distance Required
  , relative :: DataElem Unqueued Velo     Provided
  }

-- | The radar controller simulates a target vehicle sensor. By reading the
-- distance to the target vehicle from the environment (for instance Simulink),
-- @radarCtrl@ computes the relative velocity between the vehicle and the target
-- vehicle. The component is parametric in the sample time resolution.
--
-- TODO: This is not right
radarCtrl :: Time 
          -- ^ Sample time resolution
          -> AUTOSAR RadarCtrl
radarCtrl dt = atomic $ do
     state    <- interRunnableVariable 0.0
     distance <- requiredPort
     relative <- providedPort
     comSpec relative (InitValue 0.0)

     runnable (MinInterval 0) [TimingEvent dt] $ 
       do -- Ok d1 <- rteRead distance
          res <- rteRead distance
          case res of 
            Ok d1 -> do 
              Ok d0 <- rteIrvRead state
              let ds = (d1 - d0) / dt
              rteWrite relative ds
              rteIrvWrite state d1
            _ -> rteWrite relative 0.0
     return $ sealBy RadarCtrl distance relative

-- * Throttle control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Vehicle throttle controller. Limits throughput signal to the @[-1, 1]@
-- range.
throttleCtrl :: AUTOSAR (Feedthrough Throttle Throttle)
throttleCtrl = feedthrough (max (-1) . min 1) 0.0

-- * Brake control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Vehicle brake controller. Limits throughput signal to the @[0, 1]@ range.
brakeCtrl :: AUTOSAR (Feedthrough Pedal Pedal)
brakeCtrl = feedthrough (min 1) 0.0

-- * Vehicle module
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A collection of modules for import/export from/to Simulink. The external
-- software should provide brake and throttle inputs, desired cruise speed, an
-- on/off signal for the cruise control system, the current vehicle speed, and
-- traffic input (distance to the target vehicle/object ahead).
--
-- The AUTOSAR system provides throttle and brake pedal outputs.

-- | IO module for a vehicle using the ACC system.
data IOModule = IOModule
  { -- Inputs (from real world)
    cruise      :: DataElem Unqueued Velo     Required
  , velocity    :: DataElem Unqueued Velo     Required
--   , dist        :: DataElem Unqueued Distance Required
  , throttleIn  :: DataElem Unqueued Throttle Required
  , brakeIn     :: DataElem Unqueued Pedal    Required
  , onOff       :: DataElem Unqueued Double   Required
  , rpmIn       :: DataElem Unqueued Double   Required -- Engine RPM
    -- Outputs
  , throttleOut :: DataElem Unqueued Throttle Provided
  , brakeOut    :: DataElem Unqueued Pedal    Provided
  , gearOut     :: DataElem Unqueued Integer  Provided -- Gear signal
  }

instance External IOModule where
  fromExternal iom = concat 
    [ relabel "CRUISE" $ fromExternal (cruise     iom)
    , relabel "VELO"   $ fromExternal (velocity   iom)
--     , relabel "TARGET" $ fromExternal (dist       iom)
    , relabel "THR_IN" $ fromExternal (throttleIn iom)
    , relabel "BRK_IN" $ fromExternal (brakeIn    iom)
    , relabel "ONOFF"  $ fromExternal (onOff      iom)
    , relabel "RPM"    $ fromExternal (rpmIn      iom)
    ]
  
  toExternal iom = concat
    [ relabel "THR_OUT" (toExternal (throttleOut iom))
    , relabel "BRK_OUT" (toExternal (brakeOut    iom))
    , relabel "GEAR"    (toExternal (gearOut     iom))
    ]

-- | Vehicle IO module.
vehicleIO :: AUTOSAR IOModule
vehicleIO = composition $
  do -- Sample time resolution of the entire system. Inherited 
     -- by the target vehicle sensor and the ACC ECU.
     let timeStep = 1e-2
    
     -- Expose subsystems 
     accECU <- cruiseCtrl timeStep
     radar  <- radarCtrl  timeStep
     brakes <- brakeCtrl
     engine <- throttleCtrl

     connect (relative radar) (target accECU)

     -- Two-way switches for bypassing ACC. 
     -- TODO: Use /one/ bypass switch.
     bypassBrakes <- switchRoute
     bypassEngine <- switchRoute

     -- Connect switch control operations to triggers on @onOff@.
     brakesBypassTrigger <- trigger timeStep toBool 0
     engineBypassTrigger <- trigger timeStep toBool 0
     connect (switchOp bypassBrakes) (op brakesBypassTrigger)
     connect (switchOp bypassEngine) (op engineBypassTrigger)

     -- Connect switch ports
     connect (brkCtrl accECU) (switchLeft bypassBrakes)
     connect (thrCtrl accECU) (switchLeft bypassEngine)

     -- PID setup
     let diffTime = 5e-3 
         intTime  = 100
         scale    = 3e-2
     pidCtrl <- pidController timeStep diffTime intTime scale
     connect (ctrlIn accECU)     (pidInput pidCtrl) 
     connect (pidOutput pidCtrl) (ctrlOut accECU)  

     -- Gear controller setup
     gearCtrl <- gearController timeStep
     gearOut  <- providedDelegate [gearSignal gearCtrl]
     rpmIn    <- requiredDelegate [engineRPM gearCtrl]

     -- Target vehicle setup. Spawn a moving target 19 seconds in, running at
     -- 20 m/s, 100 meters ahead, alive for 30 seconds.
     moving <- movingTarget 19 20 100 30
     target <- targetCtrl timeStep
     connect (getStatus moving)      (targetOp target)
     connect (targetDistance target) (distance radar)

     -- Remaining connections 
     connect (switchOut bypassBrakes) (feedIn brakes) 
     connect (switchOut bypassEngine) (feedIn engine) 
     brakeOut    <- providedDelegate [feedOut brakes]
     throttleOut <- providedDelegate [feedOut engine]

     onOff       <- requiredDelegate [input brakesBypassTrigger, input engineBypassTrigger]
     brakeIn     <- requiredDelegate [switchRight bypassBrakes]
     throttleIn  <- requiredDelegate [switchRight bypassEngine]
--      dist        <- requiredDelegate [distance radar]
     velocity    <- requiredDelegate [vhVel accECU]
     cruise      <- requiredDelegate [crVel accECU]

     return IOModule {..}

toBool :: Double -> Bool
toBool 0 = False
toBool _ = True

-- * Adaptive Cruise Control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The adaptive cruise-control consists of a PID regulator for achieving the
-- desired cruise velocity during operations, an exported on/off operation, and
-- a radar component input which is assumed to provide the distance and relative
-- speed of some large enough object ahead of the vehicle. The on/off operation 
-- enables or bypasses the system.
--
-- When enabled, the system tries to maintain the desired cruise speed if no
-- obstacles are encountered. If the PID-controller output is negative and of a
-- large enough magnitude, the system applies brake pressure. If the system
-- detects a object on the radar with relative speed falling short of the
-- desired cruise speed, the system will provide the PID controller with this
-- speed instead. If the relative speed falls below some threshold the system is
-- allowed to panic brake.
--
-- The system thus acts as a feedthrough for throttle and brake pressure, and
-- requires radar input, desired cruise velocity as well as vehicle velocity.

-- | The ACC unit requires a target vehicle sensor, a braking controller, a
-- throttle controller, vehicle information (speed) and some user input modules;
-- i.e. a bypass switch and brake/throttle inputs.
data CruiseCtrl = CruiseCtrl
  { -- Vehicle information
    crVel   :: DataElem Unqueued Velo         Required
  , vhVel   :: DataElem Unqueued Velo         Required
    -- PID
  , ctrlIn  :: DataElem Unqueued (Velo, Velo) Provided  
  , ctrlOut :: DataElem Unqueued Throttle     Required
    -- Target vehicle sensor
  , target  :: DataElem Unqueued Velo         Required
    -- Car subsystems
  , thrCtrl :: DataElem Unqueued Throttle     Provided
  , brkCtrl :: DataElem Unqueued Pedal        Provided
  }

-- | The cruise control apparatus.
cruiseCtrl :: Time -> AUTOSAR CruiseCtrl
cruiseCtrl resolution = atomic $ 
  do -- Vehicle information
     crVel <- requiredPort
     vhVel <- requiredPort

     -- PID 
     ctrlIn  <- providedPort
     ctrlOut <- requiredPort

     -- Target vehicle sensor
     target <- requiredPort

     -- Car subsystems 
     brkCtrl <- providedPort
     thrCtrl <- providedPort
     comSpec brkCtrl (InitValue 0.0)
     comSpec thrCtrl (InitValue 0.0)

     -- Cruise control mode.
     -- ~~~~~~~~~~~~~~~~~~~~
     -- TODO: * Target speed not relative to vehicle 
     --       * Override cruise speed
     --       * Implement some sort of panic braking rule.
     --
     runnable (MinInterval 0) [TimingEvent resolution] $
       do Ok c <- rteRead crVel
          Ok v <- rteRead vhVel
          Ok r <- rteRead target

          -- Try to override cruise velocity when relative velocity to
          -- target is lower
          rteWrite ctrlIn (min c (v + r), v)
          -- rteWrite ctrlIn (c, v)
          Ok ctrl <- rteRead ctrlOut
          if ctrl < 0 then
            do printlog "ACC" $ "brakes: " ++ show (-ctrl)
               rteWrite thrCtrl 0
               rteWrite brkCtrl (-ctrl)
          else 
            do printlog "ACC" $ "throttle: " ++ show ctrl
               rteWrite thrCtrl ctrl
               rteWrite brkCtrl 0
     return $ sealBy CruiseCtrl crVel vhVel ctrlIn ctrlOut target thrCtrl brkCtrl

