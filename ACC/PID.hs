-- | AUTOSAR PID controller.
--
-- TODO: Apply digital low-pass filter to input; somewhat sensitive to noise/
--       rapid changes.
--
module PID 
  ( -- * Full PID controller
    PIDCtrl(..)
  , pidController
    -- * PD controller
  , pdController
    -- * PI controller
  , piController
  ) where

import NewARSim

-- * PID controller
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The PID controller produces a linear combination of Propotional-, 
-- Integrating and Differentiating gain. A few useful special cases (PI-, PD-)
-- are exported as well.

-- | The PID controller requires a @(cruise, vehicle)@ velocity tuple and
-- provides a throttle control.
newtype PIDCtrl = PIDCtrl
  { pidOp :: ClientServerOp (Double, Double) Double Provided }

-- | Create a PID controller.
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
     pidOp <- providedPort
     serverRunnable (MinInterval 0) [OperationInvokedEvent pidOp] $
       \(ctrl, feedback) -> do
          Ok (prevErr, prevInt) <- rteIrvRead state
         
          let err        = ctrl - feedback
              derivative = (err - prevErr) / dt
              integral   = prevInt + dt / ti * err
              output     = k * (err + integral + td * derivative) 
          rteIrvWrite state (err, integral) 
          return output
     return $ sealBy PIDCtrl pidOp

-- | Create a PD controller (proportional and derivative control).
pdController :: Time 
             -- ^ Sample time
             -> Time
             -- ^ Derivative time
             -> Double 
             -- ^ Proportional scale
             -> AUTOSAR PIDCtrl
pdController deltaT td = pidController deltaT td (1/0)

-- | Create a PI controller (proportional and integral control).
piController :: Time 
             -- ^ Sample time
             -> Time 
             -- ^ Integral time
             -> Double 
             -- ^ Proportional scale
             -> AUTOSAR PIDCtrl
piController deltaT = pidController deltaT 0

-- * Digital low-pass filter
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- TODO: FIR filter seems simple
