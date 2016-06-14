{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Adaptive Cruise Control (ACC) component in the AUTOSAR monad.
--
-- The system includes a simulated target vehicle sensor, a PID controller for 
-- regulating vehicle speed, and an ACC component which composes all inputs and
-- sub-systems.
--
-- The system requires access to the brake and throttle controlling subsystems of 
-- the car, as well as /some/ input of the distance to the target vehicle. An on/
-- off switch is provided by reading an external double input (which is converted
-- to a @Bool@ and regarded as @True@ whenever non-zero). The on/off switch
-- essentially enables or bypasses the system.
--
-- *** TODO *** 
--
--  * Some components which are marked to receive input from external sources
--    expose queued ports with unit queue length. Revise this. It should be
--    possible to trigger on data-received events w/o queued ports. Otherwise,
--    just let these components inherit sample time.
--  
--  * Fix the bug in the S-function when one output causes termination. Not sure
--    why this happens, but it likely has to do with the protocol (ARSim seems
--    to think it has received a @DIE@ message from C). Touching up the
--    communications protocol should be done in any case.
--
--  * Manually assigning the in-/outputs of a system such as this in Simulink is
--    essentially done blindfolded and is getting a bit tedious. Revise the
--    @External@ typeclass to require @String@ labels when exporting things.
--    The communications protocol (more specifically, the handshake) will then
--    be revised to send back an array of null-terminated @CString@s which the
--    S-function could either just print out to the command window together with
--    their corresponding indices, or manipulate the S-function mask to label
--    the outputs automatically (this is possible using a hack).
--
--  * The final piece of the puzzle before the simplified ACC system can be
--    tested is to somehow figure out how the ACC system can be bypassed on the
--    fly, i.e. just ignore outputs from the PID controller, and instead just
--    let the brake/throttle signals pass through w/o modification. This might
--    be possible to do with a feedthrough component which exports some sort of
--    ClientServerOp (or whatever it's called) which switches from sending data
--    from one output to another.
module ACC 
  ( -- * Cruise control
    CruiseCtrl(..)
  , cruiseCtrl
    -- * Vehicle IO module
  , IOModule(..)
  , vehicleIO
    -- * Types
  , Velo, Throttle, Distance
  ) where

import Control.Monad
import Generic
import NewARSim

-- * Types 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- These are essentially unitless (in the sense that it does not matter), so
-- long as all values come from sources of the same unit. For simplicity, use SI
-- units (i.e. m/s for velocity, rad/s for angular velocity, etc, etc).

-- | (Vehicle) velocity.
type Velo = Double

-- | Throttle application. Unitless/fraction.
type Throttle = Double

-- | Some distance measure.
type Distance = Double

-- | Brake pedal application. Unitless/fraction.
type Pedal = Double

-- * PID controller
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | The PID controller requires a @(cruise, vehicle)@ velocity tuple and
-- provides a throttle control.
data PIDCtrl c = PIDCtrl
  { pidInput  :: DataElement Queued   (Velo, Velo) Required c
  , pidOutput :: DataElement Unqueued Throttle     Provided c
  }

-- | The PID controller produces a linear combination of Propotional-, 
-- Integrating and Differentiating gain. The component requires the previous
-- state of the variables and is thus stateful. The function is parametric in 
-- sample-, integration- and differentiation time (scales), as well as a scale
-- parameter (fixed).
--
-- The system is designed to use queued inputs (although with a unit queue
-- length) to enable that the system triggers on input events and lies dormant
-- otherwise (i.e. no change in input).
pidController :: Time 
              -- ^ Sample time
              -> Time
              -- ^ D-step time scale
              -> Time
              -- ^ I-step time scale
              -> Double
              -- ^ Scale factor K
              -> Atomic c (PIDCtrl c)
              -- ^ Throttle control (output)
pidController st dt it scale =
  do state <- interRunnableVariable (0.0, 0.0)

     pidInput  <- requiredPort
     pidOutput <- providedPort 
     comSpec pidInput  (QueueLength 1)
     comSpec pidOutput (InitValue 0.0)

     runnable (MinInterval 0) [DataReceivedEvent pidInput] $
       do Ok (ctrl, feedback)   <- rteReceive pidInput
          Ok (prevInp, prevSum) <- rteIrvRead state
          
          let newInp = ctrl - feedback
              step   = newInp - prevInp
              newSum = prevSum + st / it * newInp
              newOut = scale * (newInp + prevSum + dt / st * step) 
          rteIrvWrite state (newInp, newSum) 
          rteWrite pidOutput newOut
     return PIDCtrl {..} 

-- * Target vehicle sensor
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A basic simulated radar. The system is intended to simulate the distance to
-- some moving object ahead of the vehicle, and report both relative speed as
-- well as distance to the object ahead. 

-- | Radar controller exports.
data RadarCtrl c = RadarCtrl
  { distance :: DataElement Unqueued Distance Required c
  , relative :: DataElement Unqueued Velo     Provided c
  }

-- | The radar controller simulates a target vehicle sensor. By reading the
-- distance to the target vehicle from the environment (for instance Simulink),
-- @radarCtrl@ computes the relative velocity between the vehicle and the target
-- vehicle. The component is parametric in the sample time resolution.
radarCtrl :: Time 
          -- ^ Sample time resolution
          -> Atomic c (RadarCtrl c)
radarCtrl dt =
  do state    <- interRunnableVariable 0.0
     distance <- requiredPort
     relative <- providedPort

     runnable (MinInterval 0) [TimingEvent dt] $ 
       do Ok d1 <- rteRead distance
          Ok d0 <- rteIrvRead state
          let ds = (d1 - d0) / dt
          rteWrite relative ds
          rteIrvWrite state d1
     return RadarCtrl {..}

-- * Throttle control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This component is here in case the ACC component is extended. Right now it
-- just acts as a feedthrough for throttle. It could optionally apply some sort
-- of saturation/normalisation of the signal to the @[0, 1]@ range.

-- | Vehicle throttle controller.
throttleCtrl :: Atomic c (Feedthrough c Throttle Throttle)
throttleCtrl = feedthrough return 0.0

-- * Brake control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This component is here in case the ACC component is extended. Right now it
-- just acts as a feedthrough for pedal application. It could optionally apply 
-- some sort of saturation/normalisation of the signal to the @[0, 1]@ range.

-- | Vehicle brake controller.
brakeCtrl :: Atomic c (Feedthrough c Pedal Pedal)
brakeCtrl = feedthrough return 0.0

-- * Vehicle module
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A collection of modules for import/export from/to Simulink. The external
-- software should provide brake and throttle inputs, desired cruise speed, an
-- on/off signal for the cruise control system, the current vehicle speed, and
-- traffic input (distance to the target vehicle/object ahead).
--
-- The AUTOSAR system provides throttle and brake pedal outputs.

-- | IO module for a vehicle using the ACC system.
--
-- *** TODO *** 
--   * Set @Provided@/@Required@ flags.
data IOModule = IOModule
  { -- Inputs (from real world)
    cruise      :: DataElem Unqueued Velo     Provided
  , velocity    :: DataElem Unqueued Velo     Provided
  , dist        :: DataElem Unqueued Distance Provided
  , throttleIn  :: DataElem Unqueued Throttle Provided
  , brakeIn     :: DataElem Unqueued Pedal    Provided
  , onOff       :: DataElem Unqueued Double   Provided
    -- Outputs
  , throttleOut :: DataElem Unqueued Throttle Required
  , brakeOut    :: DataElem Unqueued Pedal    Required
  }

instance External IOModule where
  fromExternal iom = concat 
    [ fromExternal (cruise     iom)
    , fromExternal (velocity   iom)
    , fromExternal (dist       iom)
    , fromExternal (throttleIn iom)
    , fromExternal (brakeIn    iom)
    , fromExternal (onOff      iom)
    ]
  
  toExternal iom = toExternal (throttleOut iom) ++ toExternal (brakeOut iom)

-- | Vehicle IO module.
vehicleIO :: AUTOSAR IOModule
vehicleIO = composition $
  do -- Sample time resolution of the entire system. Inherited 
     -- by the target vehicle sensor and the ACC ECU.
     let timeStep = 1e-2
    
     -- Required ports
     brakeOut    <- requiredPort
     throttleOut <- requiredPort

     -- Provided ports
     brakeIn     <- providedPort
     cruise      <- providedPort
     dist        <- providedPort
     onOff       <- providedPort
     throttleIn  <- providedPort 
     velocity    <- providedPort 

     -- Expose subsystems 
     accECU <- cruiseCtrl timeStep
     radar  <- radarCtrl  timeStep
     switch <- converter 
     brakes <- brakeCtrl
     engine <- throttleCtrl

     -- Create connections 
     connect dist             (distance radar)
     connect brakeIn          (brkIn accECU)
     connect throttleIn       (thrIn accECU)
     connect velocity         (vhVel accECU)
     connect cruise           (crVel accECU)
     connect (feedOut engine) throttleOut 
     connect (feedOut brakes) brakeOut
     connect (brkCtrl accECU) (feedIn brakes)
     connect (thrCtrl accECU) (feedIn engine)
     connect (relative radar) (target accECU)

     -- Create and connect control switch from @onOff@
     connect onOff            (feedIn switch)
     connect (feedOut switch) (switch accECU)

     return $ seal IOModule {..}

-- | Create a control switch from a @Double@ input.
converter :: Atomic c (Feedthrough c Double Control)
converter = feedthrough (return . toControl) Bypassed
  where
    toControl 0 = Bypassed
    toControl _ = Active

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
    crVel   :: DataElem Unqueued Velo     Required
  , vhVel   :: DataElem Unqueued Velo     Required
    -- User input modules
  , thrIn   :: DataElem Unqueued Throttle Required  
  , brkIn   :: DataElem Unqueued Pedal    Required
  , switch  :: DataElem Unqueued Control  Required 
    -- Target vehicle sensor
  , target  :: DataElem Unqueued Velo     Required
    -- Car subsystems
  , thrCtrl :: DataElem Unqueued Throttle Provided
  , brkCtrl :: DataElem Unqueued Pedal    Provided
  }

data Control = Active | Bypassed
  deriving (Data, Typeable)

-- | The cruise control apparatus.
cruiseCtrl :: Time -> AUTOSAR CruiseCtrl
cruiseCtrl resolution = composition $ 
  do -- Vehicle information
     crVel <- requiredPort
     vhVel <- requiredPort

     -- User input modules
     thrIn  <- requiredPort
     brkIn  <- requiredPort
     switch <- requiredPort

     -- Target vehicle sensor
     target <- requiredPort

     -- Car subsystems 
     thrCtrl <- providedPort
     brkCtrl <- providedPort

     -- PID setup
     let sampleTime = 1e-2   -- P-thing sample scale
         diffTime   = 0      -- D-thing sample scale
         intTime    = 1      -- I-thing sample scale
         scale      = 2      -- Output scale

     PIDCtrl pidIn pidOut <- pidController sampleTime 
                                           diffTime 
                                           intTime 
                                           scale
     
     runnable (MinInterval 0) [TimingEvent resolution] $ 
       do Ok status <- rteRead switch
          case status of 
            Active   -> return ()
            Bypassed -> return ()
     return $ seal CruiseCtrl {..}

--      runnable (MinInterval 0) [TimingEvent resolution] $
--        do Ok c <- rteRead cruise
--           Ok v <- rteRead vehicle
-- 
--           printlog "cruise_control" $ "requested cruise = " ++ show c
--           printlog "cruise_control" $ "vehicle speed    = " ++ show v
-- 
--           rteSend pidIn (c, v)
--           return ()

