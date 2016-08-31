-- | Automatic transmission for Simulink 4-speed gearbox.
--
-- The transmission component 'gearController' runs on a set sample time
-- (preferably but not necessarily the same as the rest of the system) and
-- periodically reads engine RPM and attempts to shift up/down based on this.
--
-- The code is not intended as a faithful model of an automatic transmission,
-- but is rather there to give us larger freedom with speed settings in
-- Simulink, since the vehicle's engine model is somewhat sensitive to RPM.
module AUTOSAR.ACC.Gearbox 
  ( GearCtrl(..)
  , gearController
  ) where

import AUTOSAR.ARSim
import AUTOSAR.Shared.Generic
import Control.Monad

-- | The gear logic requires engine rpm as input, and provides a gear signal
-- in the range @[-1, 4]@ as output.
data GearCtrl = GearCtrl
  { engineRPM  :: DataElem Unqueued Double  Required
  , gearSignal :: DataElem Unqueued Integer Provided
  }

-- | @'gearController' dt@ creates a gear controller reading RPM inputs
-- periodically with an interval length of @dt@. 
gearController :: Time 
               -- ^ Sample time
               -> Task
               -- ^ Task assignment
               -> AUTOSAR GearCtrl
gearController deltaT task = 
  let initialGear = -1   -- Initial gear setting
      timerLimit  = 0.5  -- Time delay after a gear change has been made.
  in atomic $ 
  do engineRPM  <- requiredPort
     gearSignal <- providedPort
     comSpec gearSignal (InitValue initialGear)
   
     -- Initial setup: Gearbox in neutral and timer enabled to prevent
     -- gear lock-in before Simulink vehicle model stabilizes.
     revs  <- interRunnableVariable 0.0
     gears <- interRunnableVariable initialGear
     timer <- interRunnableVariable (True, 0.8 * timerLimit)

     runnableT [task] (MinInterval 0) [TimingEvent deltaT] $ 
       do res <- rteRead engineRPM
          case res of 
            Ok rpm1 -> do
              Ok rpm0  <- rteIrvRead revs
              Ok gear0 <- rteIrvRead gears

              -- Check timer, we want delays between shifts to let the engine
              -- adjust revs.
              Ok (counting, t) <- rteIrvRead timer
              case (counting, t >= timerLimit) of
                (True, True)  -> 
                  do rteIrvWrite timer (False, 0.0)
                     rteWrite gearSignal gear0
                (True, False) -> 
                  do rteIrvWrite timer (True, t + deltaT)
                     rteWrite gearSignal gear0
                (False, _)    -> 
                  do let gear1 = nextGear rpm1 rpm0 gear0

                     rteIrvWrite gears gear1
                     rteIrvWrite revs  rpm1
                     unless (gear1 == gear0) $ do
                       rteIrvWrite timer (True, 0.0)
                       return ()

                     rteWrite gearSignal gear1

            _ -> rteWrite gearSignal 1 
     return $ sealBy GearCtrl engineRPM gearSignal

-- | Set new gear based on RPM and previous gear.
nextGear :: (Eq a, Ord a, Num a) => Double -> Double -> a -> a
nextGear rpm1 rpm0 gear 
  |     revUp && shiftUpI   &&     neutral          = 1
  |     revUp && shiftUp    && not high             = gear + 1
  |     revUp && shiftUp2   && gear < 3             = gear + 2
  | not revUp && shiftDown  && not (low || neutral) = gear - 1
  | not revUp && shiftDownI && low                  = -1
  | otherwise                                       = gear
  where
    revUp      = rpm1 >  rpm0
    neutral    = gear == -1
    low        = gear == 1
    high       = gear == 4
    shiftUp2   = rpm1 >  3000
    shiftUp    = rpm1 >  2500
    shiftDown  = rpm1 <  1700
    shiftUpI   = rpm1 >  1100
    shiftDownI = rpm1 <  900

