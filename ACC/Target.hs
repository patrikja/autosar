module Target 
  ( -- * Magic moving target
    MovingTarget(..)
  , movingTarget
  ) where

import Control.Monad
import Data.Maybe           (fromMaybe)
import NewARSim      hiding (void)

type Velo = Double
type Dist = Double

-- Model some sort of target vehicle which will appear very suddenly at some
-- set distance ahead with a set relative speed to our vehicle. If someone gets
-- imaginative later, this could be changed to something more complex.

data MovingTarget = MovingTarget 
  { targetStatus  :: DataElem Unqueued (Maybe (Dist, Velo)) Provided 
  , inputVelocity :: DataElem Unqueued Dist                 Required
  }

-- | TODO: If we feed this our vehicle velocity and a starting distance, we 
-- could approximate when a crash occurs. 
movingTarget :: Time 
             -- ^ Sample time
             -> Time
             -- ^ Time of appearance (s)
             -> Velo
             -- ^ Velocity
             -> Dist
             -- ^ Starting distance relative to us
             -> Time
             -- ^ Lifetime
             -> AUTOSAR MovingTarget
movingTarget deltaT t0 v s0 dur = atomic $
  do time  <- interRunnableVariable 0
     dist  <- interRunnableVariable s0
     count <- interRunnableVariable (0 :: Int)

     inputVelocity <- requiredPort
     targetStatus  <- providedPort
     comSpec targetStatus (InitValue Nothing)

     runnable (MinInterval 0) [TimingEvent deltaT] $
       do Ok t  <- rteIrvRead time
          Ok s  <- rteIrvRead dist
          Ok vf <- rteRead inputVelocity

          let s1 = s + (v - vf) * deltaT
         
          -- Some debug
          when (s > 0 && s1 <= 0) $ printlog "Target" "Crash."

          -- Log new distance only if we're in the correct interval
          rteIrvWrite time (t + deltaT)
          if t >= t0 && t < t0 + dur then do
            Ok n <- rteIrvRead count
            rteIrvWrite count ((n + 1) `mod` (truncate (1/deltaT/10)))
            when (n == 0) $ printlog "Target" $ "Distance: " ++ show s1

            rteWrite targetStatus $ Just (v, s1)
            rteIrvWrite dist s1 
          else
            rteWrite targetStatus Nothing

     return $ sealBy MovingTarget targetStatus inputVelocity

