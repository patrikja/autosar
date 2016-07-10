-- | Gearbox logic for the ACC model.
--
-- Simscape offers a 4-speed transmission which provides input- and output
-- rotational ports and schedules clutches based on an input gear selection
-- signal. This module is intended to control the transmission by providing
-- gear selection signals based on the engine rpm differences over time.
--
-- TODO: The state machine could also read an acceleration estimate and provide
--       more natural shifting that is based on acceleration as well.
module Gearbox 
  ( GearCtrl(..)
  , gearController
  ) where

import Control.Monad
import Generic
import NewARSim      hiding (void)

-- | The gear logic requires engine rpm as input, and provides a gear signal
-- in the range @[-1, 4]@ as output.
data GearCtrl = GearCtrl
  { engineRPM  :: DataElem Unqueued Double  Required
  , gearSignal :: DataElem Unqueued Integer Provided
  }

-- | @'gearController' dt@ creates a gear controller polling for inputs on 
-- intervals of length @dt@.
--
-- The gear controller produces output signals based on engine rpm. Outputs
-- range from @-1@ to @4@, where @-1@ is either reverse or neutral, and @4@ is
-- either reverse or neutral. Anything in between should be as expected.
gearController :: Time
               -- ^ Sample time 
               -> AUTOSAR GearCtrl
gearController resolution = 
  let initialGear = -1
      timerLimit  = 1.0 
  in atomic $ 
  do engineRPM  <- requiredPort
     gearSignal <- providedPort
     comSpec gearSignal (InitValue initialGear)
    
     revs  <- interRunnableVariable 0.0
     gears <- interRunnableVariable initialGear
     timer <- interRunnableVariable (False, 0.0)

     runnable (MinInterval 0) [TimingEvent resolution] $
       do res <- rteRead engineRPM
          case res of 
            Ok rpm1 -> do
              Ok rpm0  <- rteIrvRead revs
              Ok gear0 <- rteIrvRead gears

              -- Check timer, we want delays between shifts to let the engine
              -- adjust revs.
              Ok (counting, t) <- rteIrvRead timer
              -- One second delay:
              case (counting, t >= timerLimit) of
                (True, True)  -> 
                  do rteIrvWrite timer (False, 0.0)
                     rteWrite gearSignal gear0
                (True, False) -> 
                  do rteIrvWrite timer (True, t + resolution)
                     rteWrite gearSignal gear0
                (False, _)    -> 
                  do let gear1 = setGear rpm1 rpm0 gear0

                     rteIrvWrite gears gear1
                     rteIrvWrite revs  rpm1
                     unless (gear1 == gear0) $ void $ 
                       rteIrvWrite timer (True, 0.0)
                     rteWrite gearSignal gear1

            _ -> rteWrite gearSignal 1 
     return $ sealBy GearCtrl engineRPM gearSignal

-- | Set new gear based on RPM and previous gear.
setGear :: (Eq a, Ord a, Num a) => Double -> Double -> a -> a
setGear rpm1 rpm0 gear 
  |     revUp && shiftUpI   &&     neutral          = 1
  |     revUp && shiftUp    && not high             = gear + 1
  | not revUp && shiftDown  && not (low || neutral) = gear - 1
  | not revUp && shiftDownI && low                  = -1
  | otherwise                                       = gear
  where
    revUp      = rpm1 >  rpm0
    neutral    = gear == -1
    low        = gear == 1
    high       = gear == 4
    shiftUp    = rpm1 >  4000
    shiftDown  = rpm1 <  2000
    shiftUpI   = rpm1 >  1100
    shiftDownI = rpm1 <  900

-- * Engine rev-limiter.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Simple rev-limiting logic.
--
-- TODO: Finish.

data RevLimit = RevLimit
  { rpmIn          :: DataElem Unqueued Double Required
  , revThrottleIn  :: DataElem Unqueued Double Required
  , revThrottleOut :: DataElem Unqueued Double Provided
  }

-- | Limits the engine speed to a fixed number of revolutions per minute. Simply
-- cuts off throttle until engine revs have dropped by some fixed percentage.
revLimit :: Time 
         -- ^ Sample time
         -> Double
         -- ^ RPM limit
         -> AUTOSAR RevLimit
revLimit resolution limit = undefined

