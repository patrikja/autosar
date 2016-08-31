-- | Limit engine revs by cutting off throttle.
module AUTOSAR.ACC.Revlimit
  ( RevLimit(..)
  , revLimit
  ) where

import AUTOSAR.ARSim

-- | The rev-limiter exports an operation which takes a @(throttle, rpm)@ tuple
-- and returns a @throttle@ value.
newtype RevLimit = RevLimit
  { revOp :: ClientServerOp (Double, Double) Double Provided }

-- | Limits the engine speed to a fixed number of revolutions per minute. Simply
-- cuts off throttle until engine revs have dropped below threshold. 
revLimit :: Double -> AUTOSAR RevLimit
revLimit revLimit = atomic $
  do revOp <- providedPort
     serverRunnable (MinInterval 0) [OperationInvokedEvent revOp] $ 
       \(throttle, rpm) -> 
         return $ if rpm >= revLimit then -1 else throttle
     return $ sealBy RevLimit revOp

