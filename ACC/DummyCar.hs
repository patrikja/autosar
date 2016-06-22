-- | Dummy car. This module does not work with the revised cruise-control 
-- component.

{-# LANGUAGE RecordWildCards #-}

module Main where

import ACC
import Control.Monad
import Graphics.EasyPlot
import NewARSim          hiding (void)

data Car = Car
  { pedal :: DataElem Unqueued Throttle Required
  , velo  :: DataElem Unqueued Velo     Provided
  , ctrl  :: DataElem Unqueued Velo     Provided
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
  do pedal <- requiredPort
     velo  <- providedPort
     ctrl  <- providedPort

     probeWrite "velocity" velo
     probeWrite "cruise"   ctrl

     comSpec velo (InitValue initVel)
     comSpec ctrl (InitValue initCruise)
     
     state <- interRunnableVariable (initVel, initCruise, 0.0) 
     runnable (MinInterval 0) [TimingEvent resolution] $
       do Ok (vel, cru, time) <- rteIrvRead state
          std <- rteRead pedal
          case std of 
            Ok throttle -> 
              do let newTime = time + resolution
                     newCru  = cru
                     newVel  = accelerate vel (incline time) throttle
                 rteWrite velo newVel
                 rteWrite ctrl newCru
                 rteIrvWrite state (newVel, newCru, newTime)
                 return ()
            _ -> 
              do printlog "CAR" $ show std
                 return ()
     return $ sealBy Car pedal velo ctrl

-- | Compute incline based on time.
incline :: Time -> Double
incline t = pi / 4 * sin (t * pi)

-- | Accelerate (or brake) car based on previous speed, incline and throttle.
accelerate :: Velo -> Double -> Throttle -> Velo
accelerate velo rads throttle = velo + dv + throttle * scale - 1
  where
    dv    = (-rads) / pi / 10
    scale = 1e-2

-- * Plotting
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

makePlot :: Trace -> IO Bool
makePlot trace = plot X11 curves
  where curves  = [ Data2D [Title str, Style Lines, Color (color str)] [] 
                    (discrete pts)
                  | (str,pts) <- ms]
        color "PID OUT"  = Red
        color "V"        = Blue
        color "C"        = Green
        color _          = Black
        ms               = doubles
        doubles :: [(ProbeID, Measurement Double)]
        doubles          = probeAll trace


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
  do pid <- pidController 1e-2 0 1 2 
     acc <- cruiseCtrl resolution
     car <- dummyCar 

     -- Connect PID
     connect (ctrlIn acc)    (pidInput pid) 
     connect (pidOutput pid) (ctrlOut acc)  

     -- Connect car
     connect (thrCtrl acc) (pedal car)
--      r <- delegateP [thrCtrl acc, brkCtrl acc]
--      connect r (pedal car)
     connect (velo  car)   (vhVel acc)
     connect (ctrl car)    (crVel acc)

main :: IO ()
main = simulateStandalone 15.0 output (RandomSched (mkStdGen 111)) test
  where 
    output trace = void $
      do printLogs trace 
         makePlot trace

