
-- | Adaptive Cruise Control (ACC) component in the AUTOSAR monad. Current
-- status: Old-school cruise control sandbox.
module Main where

import Control.Monad
import DummyCar
import Graphics.EasyPlot
import NewARSim
import Sequencer

-- * Types 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Velo     = Double
type Accel    = Double
type Throttle = Double

-- * PID controller
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | The PID controller requires a @(cruise, vehicle)@ velocity tuple and
-- provides a throttle control.
data PIDCtrl c = PIDCtrl
  { ctrl   :: DataElement Queued   (Velo, Velo) Required c
  , output :: DataElement Unqueued Throttle     Provided c
  }

-- | The PID controller produces a linear combination of Propotional-, 
-- Integrating and Differentiating gain. The component requires the previous
-- state of the variables and is thus stateful.
--
-- The function is parametric in sample-, integration- and differentiation time
-- (scales), as well as a scale parameter (fixed).
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
  do -- Previous input and cumulative sum. 
     state <- interRunnableVariable (0.0, 0.0)

     output <- providedPort 
     ctrl   <- requiredPort
     comSpec ctrl   (QueueLength 2)
     comSpec output (InitValue 0.0)

     runnable (MinInterval 0) [DataReceivedEvent ctrl] $
       do Ok (c, f) <- rteReceive ctrl
          Ok (prevInp, prevSum) <- rteIrvRead state
          
          let newInp = c - f
              step   = newInp - prevInp
              newSum = prevSum + st / it * newInp
              newOut = scale * (newInp + prevSum + dt / st * step) 
          rteIrvWrite state (newInp, newSum) 
          rteWrite output newOut
     return $ PIDCtrl ctrl output 

-- * Basic cruise control
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The basic cruise-control setup consists of a comparator and throttle gain. 
-- When active, the unit reads an approximation of the vehicle speed and
-- compares this to the desired cruise speed (signed difference). A PID-
-- controller is used to compute a throttle gain from the signed difference.
--
-- For simplicity we assume that the throttle is a double in the range @[0, 1]@.
--
-- *** TODO ***
--   * Normalize/saturate throttle.
--   * On/off flag.

-- | Cruise control exports.
data CruiseCtrl = CruiseCtrl
  { cvel     :: DataElem Unqueued Velo     Required
  -- ^ Cruise velocity (control input)
  , vvel     :: DataElem Unqueued Velo     Required
  -- ^ Vehicle velocity (feedback)
  , throttle :: DataElem Unqueued Throttle Provided
  -- ^ Throttle control (output)
  }

-- | The cruise control apparatus.
cruiseCtrl :: AUTOSAR CruiseCtrl
cruiseCtrl = composition $ 
  do vehicle  <- requiredPort
     cruise   <- requiredPort
     pidInput <- providedPort

     let sampleTime = 1e-2
         diffTime   = 0
         intTime    = 2
         scale      = 1
         resolution = 1e-2

     pid <- pidController sampleTime diffTime intTime scale
     connect pidInput (ctrl pid)

     runnable (MinInterval 0) [TimingEvent resolution] $
       do Ok c <- rteRead cruise
          Ok v <- rteRead vehicle

          printlog "CC" $ "cruise  " ++ show c
          printlog "CC" $ "vehicle " ++ show v

          rteSend pidInput (c, v)
          return ()
     return $ sealBy CruiseCtrl cruise vehicle (output pid)

-- * External simulation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

instance External CruiseCtrl where
  fromExternal (CruiseCtrl c v _) = fromExternal c ++ fromExternal v 
  toExternal   (CruiseCtrl _ _ t) = toExternal t

-- * Stand-alone simulation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Stand-alone simulation using some simulated dummy car just to get some output
-- from the program.

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
    output trace = 
      do printLogs trace 
         makePlot trace
         return ()

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

