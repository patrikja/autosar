module Target 
  ( -- * Magic moving target
    MovingTarget(..)
  , movingTarget
    -- * Types
  , Distance
  ) where

import Control.Monad
import Data.Maybe           (fromMaybe)
import NewARSim      hiding (void)

-- | Some distance measure.
type Distance = Double

-- Model some sort of target vehicle which will appear very suddenly at some
-- set distance ahead, travelling at a set speed, for some duration.

newtype MovingTarget = MovingTarget 
  { targetStatus :: DataElem Unqueued (Maybe Distance) Provided }

-- | @'movingTarget' t0 v s0 dur@ creates a moving target which mystically appears
-- @s0@ meters ahead of the caller at time @t0@, travelling at a speed of @v@ 
-- m/s, and exists for a duration @dur@ before it magically disappears.
movingTarget :: Time 
             -- ^ Sample time
             -> Time
             -- ^ Time of appearance (s)
             -> Double
             -- ^ Velocity (m/s)
             -> Distance
             -- ^ Position at appearance (m)
             -> Time
             -- ^ Lifetime
             -> AUTOSAR MovingTarget
movingTarget deltaT t0 v s0 dur = atomic $
  do position     <- interRunnableVariable s0
     time         <- interRunnableVariable 0
     targetStatus <- providedPort
     comSpec targetStatus (InitValue Nothing)

     runnable (MinInterval 0) [TimingEvent deltaT] $
       do Ok t <- rteIrvRead time
          Ok s <- rteIrvRead position
          
          let status = if t >= t0 && t < t0 + dur then
                         Just (s + v * deltaT)
                       else
                         Nothing

          rteIrvWrite position     (fromMaybe s status)
          rteIrvWrite time         (t + deltaT)
          rteWrite    targetStatus status
         
     return $ sealBy MovingTarget targetStatus 

