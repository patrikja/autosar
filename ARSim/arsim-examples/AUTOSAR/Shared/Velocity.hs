{-# LANGUAGE DeriveDataTypeable #-}

module AUTOSAR.Shared.Velocity 
  ( VelocityCtrl(..)
  , velocityCtrl
  ) where

import AUTOSAR.ARSim 
import Control.Monad

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
             -> Task
             -- ^ Task assignment
             -> AUTOSAR VelocityCtrl
velocityCtrl deltaT task = atomic $ 
  do wheels   <- replicateM 4 requiredPort
     accel    <- requiredPort 
     velocity <- providedPort
      
     mapM_ (`comSpec` InitValue 0.0) wheels 
     comSpec accel    (InitValue 0.0)
     comSpec velocity (InitValue 0.0)
      
     let memory = 200
    
     -- Keep acceleration memory for backwards integration phase.
     accMem  <- interRunnableVariable (replicate memory 0.0)
     avgMem  <- interRunnableVariable (replicate memory 0.0)
     veloMem <- interRunnableVariable 0.0 
     state   <- interRunnableVariable S0

     runnableT [task] (MinInterval 0) [TimingEvent deltaT] $ 
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

data CState = SM2 | SM1 | S0 | S1
  deriving (Data, Eq, Typeable)

-- Vehicle state machine transitions.
stateTrans :: Double -- Average velocity
           -> Double -- Longtitudal acceleration
           -> CState -- Input state
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
    vMin   = 0.01             -- Low velocity threshold  (m/s)
    hv     = 0.2              -- Velocity hysteresis     (m/s)
    ha     = 0.1              -- Acceleration hysteresis (m/s^2)
    beta   = 0.8              -- Deceleration threshold  (m/s^2)
    delta  = 0.1              -- Acceleration threshold  (m/s^2)


