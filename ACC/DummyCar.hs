module DummyCar 
  ( dummyCar
  , Car(..)
  ) where

import NewARSim

data Car = Car
  { pedal  :: DataElem Unqueued Double Required
  , velo   :: DataElem Unqueued Double Provided
  , cruise :: DataElem Unqueued Double Provided
  }

-- Initial velocity (km/h).
initVel :: Double
initVel = 10

-- Initial control velocity (km/h).
initCruise :: Double
initCruise = 50

-- The sample time resolution of the dummy car.
resolution :: Double
resolution = 1e-2

-- | A dummy car. The car is running in some sine shaped one dimensional 
-- landscape and requires a pedal control port to engage/disengage throttle.
-- The vehicle acceleration is a function of the throttle and the road incline.
-- We assume that the engine is powerful enough to brake the car; we're only
-- interested to see how throttle is applied dynamically as a response to the
-- car velocity. The cruise velocity is set to a constant for now.
dummyCar :: AUTOSAR Car 
dummyCar = atomic $ 
  do ped <- requiredPort
     vv  <- providedPort
     cv  <- providedPort

     probeWrite "throttle" ped
     probeWrite "velocity" vv
     probeWrite "cruise"   cv

     comSpec vv (InitValue initVel)
     comSpec cv (InitValue initCruise)
     
     state <- interRunnableVariable (initVel, initCruise, 0.0) 
     runnable (MinInterval 0) [TimingEvent resolution] $
       do Ok (vel, cru, time) <- rteIrvRead state
          Ok throttle <- rteRead ped
          let newTime = time + resolution
              newCru  = cru
              newVel  = accelerate vel (incline time) throttle
          rteWrite vv newVel
          rteWrite cv newCru
          rteIrvWrite state (newVel, newCru, newTime)
          return ()
     return $ sealBy Car ped vv cv

-- | Compute incline based on time.
incline :: Double -> Double
incline t = pi / 4 * sin (t * pi)

-- | Accelerate car based on previous speed, incline and throttle.
--
-- *** TODO *** 
--   * This is mostly to test that the ACC program actually runs and produces
--     some sort of output. It will quickly be replaced by a simulated car in
--     Simulink as soon as I find the old version which had a working engine,
--     but ...
--   * ... it's still interesting to figure out how to do this in a reasonably
--     good way.
accelerate :: Double -> Double -> Double -> Double
accelerate velo rads throttle = velo + dv + throttle / 1e2
  where
    dv = (-rads) / pi / 10

