module Target 
  ( -- * Magic moving target
    MovingTarget(..)
  , movingTarget
    -- * A target we can hook up to
  , TargetCtrl(..)
  , targetCtrl
    -- * Types
  , Distance
  ) where

import Control.Monad
import NewARSim      hiding (void)

-- | Some distance measure.
type Distance = Double

-- Model some sort of target vehicle which will appear very suddenly at some
-- set distance ahead, travelling at a set speed, for some duration.

newtype MovingTarget = MovingTarget 
  { getStatus :: ClientServerOp () (Maybe Distance) Provided }

-- | @'movingTarget' t0 v s0 dur@ creates a moving target which mystically appears
-- @s0@ meters ahead of the caller at time @t0@, travelling at a speed of @v@ 
-- m/s, and exists for a duration @dur@ before it magically disappears.
movingTarget :: Time
             -- ^ Time of appearance (s)
             -> Double
             -- ^ Velocity (m/s)
             -> Distance
             -- ^ Position at appearance (m)
             -> Time
             -- ^ Lifetime
             -> AUTOSAR MovingTarget
movingTarget t0 v s0 dur = atomic $
  do position  <- interRunnableVariable s0
     time      <- interRunnableVariable 0
     lock      <- exclusiveArea
     getStatus <- providedPort

     -- Keep track of time
     -- TODO: Desireable to have this thing "sleep" until we reach @t0@.
     --       Very much desirable to turn it off entierly when we reach 
     --       @t0 + dur@.
     let dt = 1e-1
     runnable (MinInterval 0) [TimingEvent dt] $
       do rteEnter lock
          Ok t <- rteIrvRead time

          -- When we're in the working range, do stuff.
          when (t >= t0 && t < t0 + dur) $
            do Ok s <- rteIrvRead position
               rteIrvWrite position (s + v * dt)
               return ()

          -- Update time
          rteIrvWrite time (t + dt)
          rteExit lock
          return ()
         
     -- Serve requests for actual distance. If the vehicle is not there,
     -- all we get is @Nothing@.
     serverRunnable (MinInterval 0) [OperationInvokedEvent getStatus] $ \_ ->
       do rteEnter lock
          Ok t <- rteIrvRead time
          Ok s <- rteIrvRead position
          rteExit lock

          if t >= t0 && t < t0 + dur then
            return $ Just s
          else
            return Nothing
     return $ sealBy MovingTarget getStatus 

data TargetCtrl = TargetCtrl
  { targetOp       :: ClientServerOp () (Maybe Distance) Required
  , targetDistance :: DataElem Unqueued Distance Provided 
  }

targetCtrl :: Time -> AUTOSAR TargetCtrl
targetCtrl resolution = atomic $
  do let farAway = 500
     targetOp       <- requiredPort
     targetDistance <- providedPort
     comSpec targetDistance (InitValue farAway)

     runnable (MinInterval 0) [TimingEvent 1e-1] $ 
       do Ok res <- rteCall targetOp ()
          case res of
            Just s' -> rteWrite targetDistance s'
            Nothing -> rteWrite targetDistance farAway
          
     return $ sealBy TargetCtrl targetOp targetDistance

