-- | This module contains all vehicle-related things, such as IO/control of
-- vehicle subsystems, velocity and slip estimation, et cetera. 

{-# LANGUAGE RecordWildCards #-}

module AUTOSAR.ACC.Vehicle 
  ( IOModule(..)
  , vehicleIO
  ) where

import AUTOSAR.ABS.ABS
import AUTOSAR.ACC.ACC
import AUTOSAR.ACC.Gearbox
import AUTOSAR.ACC.Revlimit
import AUTOSAR.ARSim 
import AUTOSAR.Shared.Generic
import AUTOSAR.Shared.Velocity

import Control.Monad

type Dist = Double

-- * Radar control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data RadarCtrl = RadarCtrl
  { radarDistIn :: DataElem Unqueued Double                   Required
  , radarRelIn  :: DataElem Unqueued Double                   Required
  , radarOutput :: DataElem Unqueued (Maybe (Double, Double)) Provided
  }

radarCtrl :: Time -> Task -> Double -> AUTOSAR RadarCtrl
radarCtrl deltaT task thresh = atomic $ do
  distIn <- requiredPort
  relIn  <- requiredPort
  out    <- providedPort

  comSpec out    (InitValue Nothing)
  comSpec distIn (InitValue 100)
  comSpec relIn  (InitValue 0.0)

  runnableT [task] (MinInterval 0) [TimingEvent deltaT] $ do
    Ok d <- rteRead distIn
    Ok r <- rteRead relIn
    let output
          | d < thresh = Just (r, d)
          | otherwise  = Nothing
    rteWrite out output

  return $ sealBy RadarCtrl distIn relIn out

-- * Throttle control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data ThrottleCtrl = ThrottleCtrl
  { throttleCtrlIn  :: DataElem Unqueued Double Required
  , throttleCtrlOut :: DataElem Unqueued Double Provided
  , throttleCtrlRpm :: DataElem Unqueued Double Required
  , throttleCtrlRev :: ClientServerOp (Double, Double) Double Required
  }

-- Vehicle throttle controller. Limits throughput signal to the @[-1, 1]@
-- range. Makes use of the rev-limiter.
throttleCtrl :: Task -> AUTOSAR ThrottleCtrl 
throttleCtrl task = atomic $ do 
  thrIn  <- requiredPort
  rpmIn  <- requiredPort
  revLim <- requiredPort
  thrOut <- providedPort

  comSpec thrOut (InitValue 0.0)

  runnableT [task] (MinInterval 0) [DataReceivedEvent thrIn] $ do
    Ok thr0 <- rteRead thrIn
    Ok rpm  <- rteRead rpmIn
    Ok thr1 <- rteCall revLim (thr0, rpm)
    rteWrite thrOut ((max (-1) . min 1) thr1)
    return ()

  return $ sealBy ThrottleCtrl thrIn thrOut rpmIn revLim
  
-- * Brake control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Vehicle brake controller. Limits throughput signal to the @[0, 1]@ range.
brakeCtrl :: Task -> AUTOSAR (Feedthrough Throttle Throttle)
brakeCtrl task = feedthroughT [task] (min 1) 0.0

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
  , rpmIn       :: DataElem Unqueued Double   Required
  , wheelsIn    :: [DataElem Unqueued Velo    Required]
  , accelIn     :: DataElem Unqueued Double   Required
  , distIn      :: DataElem Unqueued Double   Required
  , relIn       :: DataElem Unqueued Double   Required
    -- Outputs
  , throttleOut :: DataElem Unqueued Throttle Provided
  , brakeOut    :: DataElem Unqueued Throttle Provided
  , gearOut     :: DataElem Unqueued Integer  Provided
  , valvesOut   :: [ValveP                    Provided]
  }

enumLabel :: [(Address, String)] -> [(Address, String)]
enumLabel is = map (\((a, l), i) -> (a, l ++ show i)) $ is `zip` [1..]

instance External IOModule where
  fromExternal iom = concat 
    [ relabel "CRUISE" $ fromExternal (cruise  iom)
    , relabel "RPM"    $ fromExternal (rpmIn   iom)
    , relabel "ACCEL"  $ fromExternal (accelIn iom)
    , relabel "DIST"   $ fromExternal (distIn  iom)
    , relabel "REL"    $ fromExternal (relIn   iom)
    , enumLabel $ relabel "WHEEL" $ fromExternal (wheelsIn iom) 
    ]
  
  toExternal iom = concat
    [ relabel "THR_OUT" (toExternal (throttleOut iom))
    , relabel "BRK_OUT" (toExternal (brakeOut    iom))
    , relabel "GEAR"    (toExternal (gearOut     iom))
    , toExternal (valvesOut iom) 
    ]

-- | Vehicle IO module.
--
-- The model is parametrized on the task assignmens of the runnables, as well as
-- the minimum sample time in the model. You will still need to take into
-- account which runnables are triggered by timers or data reception events when
-- you give a task assignment (see this module, as well as "AUTOSAR.ACC",
-- "AUTOSAR.Gearbox" and "AUTOSAR.Revlimit").
vehicleIO :: Time 
          -> [Task]
          -> AUTOSAR IOModule
vehicleIO deltaT tasks = composition $
  do -- Specific task assignments
     -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     let [veloT, absT, bangT, radarT, accT, brkT, thrT, gearT] = tasks
    
     -- Car subsystems
     -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     brakes   <- brakeCtrl                    brkT
     engine   <- throttleCtrl                 thrT
     gearCtrl <- gearController (10 * deltaT) gearT
     veloCtrl <- velocityCtrl   deltaT        veloT 
     radar    <- radarCtrl      (10 * deltaT) radarT 50
     acc      <- accSystem      (10 * deltaT) accT
     rpmLimit <- revLimit       5500
     absCtrl  <- absSystem      deltaT        absT bangT
    
     -- Connections
     connect (radarOutput radar)   (accTarget acc)
     connect (velocity veloCtrl)   (accVeloIn acc)
     connect (velocity veloCtrl)   (absVeloIn absCtrl)
     connect (accBrake acc)        (feedIn brakes) 
     connect (accThrottle acc)     (throttleCtrlIn engine) 
     connect (revOp rpmLimit)      (throttleCtrlRev engine)

     -- Delegations
     brakeOut    <- providedDelegate [feedOut brakes]
     throttleOut <- providedDelegate [throttleCtrlOut engine]
     cruise      <- requiredDelegate [accCruise acc]
     accelIn     <- requiredDelegate [accel  veloCtrl]
     gearOut     <- providedDelegate [gearSignal gearCtrl]
     distIn      <- requiredDelegate [radarDistIn radar]
     relIn       <- requiredDelegate [radarRelIn  radar]
     rpmIn       <- requiredDelegate [ engineRPM gearCtrl
                                     , throttleCtrlRpm engine
                                     ]
     
     -- Strip ports and wheels from ABS system.
     let ws = wheels veloCtrl `zip` map veloIn (wheelPorts absCtrl)
     wheelsIn  <- forM ws $ \(vc, wp) -> requiredDelegate [vc, wp]
     valvesOut <- forM (map valveOut (wheelPorts absCtrl)) $ \vp ->
       providedDelegate [vp]

     return IOModule {..}

