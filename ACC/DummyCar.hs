-- | Dummy car. This module does not work with the revised cruise-control 
-- component.
module Main where

import ACC
import Control.Monad
import Graphics.EasyPlot
import NewARSim          hiding (void)

data Car = Car
  { pedal  :: DataElem Unqueued Throttle Required
  , velo   :: DataElem Unqueued Velo     Provided
  , cruise :: DataElem Unqueued Velo     Provided
  }

-- Initial velocity (km/h).
initVel :: Velo 
initVel = 10

-- Initial control velocity (km/h).
initCruise :: Velo
initCruise = 50

-- The sample time resolution of the dummy car.
resolution :: Time
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
incline :: Time -> Double
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
accelerate :: Velo -> Double -> Throttle -> Velo
accelerate velo rads throttle = velo + dv + throttle * scale
  where
    dv    = (-rads) / pi / 10
    scale = 1e-2

-- * Plotting
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

makePlot :: Trace -> IO Bool
makePlot trace = plot X11 curves
  where curves  = [ Data2D [Title str, Style Lines, Color (color str)] [] 
                    (discrete pts)
                  | (str,pts) <- doubles ]
        color "throttle"  = Red
        color "velocity"  = Blue
        color "cruise"    = Green
        color _           = Black
        doubles :: [(ProbeID, Measurement Double)]
        doubles = probeAll trace

discrete :: Fractional t => [((q, t), k)] -> [(t, k)]
discrete []                     = []
discrete (((_,t),v):vs)         = (t,v) : disc v vs
  where disc v0 (((_,t),v):vs)  = (t,v0) : (t+eps,v) : disc v vs
        disc _ _                = []
        eps                     = 0.0001

-- * Stand-alone simulation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Stand-alone simulation using some simulated dummy car with the ACC, just to 
-- get some output from the ACC component.

test :: AUTOSAR ()
test = 
  do cc  <- cruiseCtrl 
     car <- dummyCar 
     connect (velo car)    (vvel cc)
     connect (cruise car)  (cvel cc)
     connect (throttle cc) (pedal car)

main :: IO ()
main = simulateStandalone 15.0 output (RandomSched (mkStdGen 111)) test
  where 
    output trace = void $
      do printLogs trace 
         makePlot trace

