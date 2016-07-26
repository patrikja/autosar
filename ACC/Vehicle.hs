-- | This module contains all vehicle-related things, such as IO/control of
-- vehicle subsystems, velocity and slip estimation, et cetera. Most of these
-- things were previously located in "ACC" or "NewABS2", but were required by
-- both modules.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Vehicle where

import ACC
import Control.Monad
import Gearbox
import Generic
import NewARSim 
import NewABS2
import Revlimit
import Target

type Dist = Double

-- * Throttle control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data ThrottleCtrl = ThrottleCtrl
  { throttleCtrlIn  :: DataElem Unqueued Double Required
  , throttleCtrlOut :: DataElem Unqueued Double Provided
  , throttleCtrlRpm :: DataElem Unqueued Double Required
  , throttleCtrlRev :: ClientServerOp (Double, Double) Double Required
  }

-- | Vehicle throttle controller. Limits throughput signal to the @[-1, 1]@
-- range. Makes use of the rev-limiter.
throttleCtrl :: Time -> AUTOSAR ThrottleCtrl 
throttleCtrl deltaT = atomic $ do -- feedthrough (max (-1) . min 1) 0.0
  thrIn  <- requiredPort
  rpmIn  <- requiredPort
  revLim <- requiredPort
  thrOut <- providedPort

  comSpec thrOut (InitValue 0.0)

  runnable (MinInterval 0) [DataReceivedEvent thrIn] $ do
    Ok thr0 <- rteRead thrIn
    Ok rpm  <- rteRead rpmIn
    Ok thr1 <- rteCall revLim (thr0, rpm)
    rteWrite thrOut ((max (-1) . min 1) thr1)
    return ()

  return $ sealBy ThrottleCtrl thrIn thrOut rpmIn revLim
  
-- * Brake control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Vehicle brake controller. Limits throughput signal to the @[0, 1]@ range.
brakeCtrl :: AUTOSAR (Feedthrough Throttle Throttle)
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
  , rpmIn       :: DataElem Unqueued Double   Required
  , wheelsIn    :: [DataElem Unqueued Velo    Required]
  , accelIn     :: DataElem Unqueued Double   Required
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
-- TODO: Separate ACC subsystems from ABS subsystems.
vehicleIO :: Time -> Dist -> Velo -> Time -> AUTOSAR IOModule
vehicleIO time dist velo dur = composition $
  do let timeStep = 1e-2
     
     -- Car subsystems
     -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     brakes   <- brakeCtrl
     engine   <- throttleCtrl   timeStep
     gearCtrl <- gearController timeStep
     veloCtrl <- velocityCtrl   timeStep
     acc      <- accSystem      timeStep 
     rpmLimit <- revLimit 5500
     
     moving   <- movingTarget timeStep time dist velo dur
    
     -- Connections
     connect (targetStatus moving) (accTarget acc)
     connect (velocity veloCtrl)   (accVeloIn acc)
     connect (velocity veloCtrl)   (inputVelocity moving)
     connect (accBrake acc)        (feedIn brakes) 
     connect (accThrottle acc)     (throttleCtrlIn engine) 
     connect (revOp rpmLimit)      (throttleCtrlRev engine)

     -- Delegations
     brakeOut    <- providedDelegate [feedOut brakes]
     throttleOut <- providedDelegate [throttleCtrlOut engine]
     cruise      <- requiredDelegate [accCruise acc]
     accelIn     <- requiredDelegate [accel  veloCtrl]
     gearOut     <- providedDelegate [gearSignal gearCtrl]
     rpmIn       <- requiredDelegate [ engineRPM gearCtrl
                                     , throttleCtrlRpm engine
                                     ]
     
     -- Connect wheels to ABS system /and/ velocity controller
     absCtrl <- absSystem 
     connect (velocity veloCtrl) (absVeloIn absCtrl)

     -- Strip ports and wheels from ABS system.
     let ws = wheels veloCtrl `zip` map veloIn (wheelPorts absCtrl)
     wheelsIn  <- forM ws $ \(vc, wp) -> requiredDelegate [vc, wp]
     valvesOut <- forM (map valveOut (wheelPorts absCtrl)) $ \vp ->
       providedDelegate [vp]

     return IOModule {..}

-- * Velocity estimation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | The velocity estimation algorithm requires wheel speeds and (filtered)
-- accelerometer inputs. It is assumed that the first two wheel inputs belong to
-- the non-driving wheels (if any).
data VelocityCtrl = VelocityCtrl
  { wheels   :: [DataElem Unqueued Double Required]
    -- ^ Wheel velocities
  , accel    :: DataElem Unqueued Double Required
    -- ^ Accelerometer
  , velocity :: DataElem Unqueued Double Provided
    -- ^ Velocity approximation
  }

-- | Performs vehicle velocity estimation based on vehicle longtitudal
-- acceleration and wheel speeds. Requires a longtitudal accelerometer to be
-- mounted on the vehicle. Moreover, it is assumed that the data from this
-- accelerometer has been filtered.
-- 
-- The algorithm used assumes that the first two wheel velocity inputs come from
-- the non-driving wheels of the vehicle, and that a longtitudal accelerometer
-- is mounted on the unit. The slip calculation algorithm is borrowed from 
-- Active Braking Control Systems: Design for Vehicles by Savaresi and Tanelli,
-- chapter 5.
velocityCtrl :: Time 
             -- ^ Sample time 
             -> AUTOSAR VelocityCtrl
velocityCtrl deltaT = atomic $ 
  do wheels   <- replicateM 4 requiredPort
     accel    <- requiredPort 
     velocity <- providedPort
      
     mapM_ (`comSpec` InitValue 0.0) wheels 
     comSpec accel    (InitValue 0.0)
     comSpec velocity (InitValue 0.0)
      
     let memory = 100
    
     -- Keep acceleration memory for backwards integration phase.
     accMem  <- interRunnableVariable (replicate memory 0.0)
     avgMem  <- interRunnableVariable (replicate memory 0.0)
     veloMem <- interRunnableVariable 0.0 
     state   <- interRunnableVariable S0

     runnable (MinInterval 0) [TimingEvent deltaT] $ 
       do velos  <- mapM (fmap fromOk . rteRead) wheels
          Ok acc <- rteRead accel
          Ok s0  <- rteIrvRead state
          Ok v0  <- rteIrvRead veloMem
          Ok as  <- rteIrvRead accMem
          Ok avs <- rteIrvRead avgMem
           
          -- Velocity estimation
          -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          -- Assume that accelerometer input is filtered. This could be 
          -- done here, provided that we implement a FIR filter (for instance
          -- moving average).
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
          rteIrvWrite state   s1

          -- Write velocity estimate
          rteWrite velocity v1
     return $ sealBy VelocityCtrl wheels accel velocity

fromOk (Ok v) = v

-- * Vehicle state machine 
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
    vMin   = 0.5              -- Low velocity threshold  (m/s)
    hv     = 0.2              -- Velocity hysteresis     (m/s)
    ha     = 0.1              -- Acceleration hysteresis (m/s^2)
    beta   = 0.8              -- Deceleration threshold  (m/s^2)
    delta  = 0.1              -- Acceleration threshold  (m/s^2)

