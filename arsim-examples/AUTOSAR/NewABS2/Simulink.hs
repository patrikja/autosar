module AUTOSAR.NewABS2.Simulink where

import AUTOSAR.NewABS2.NewABS2
import AUTOSAR.Shared.Velocity
import Control.Monad
import NewARSim
import System.Random

enumLabel :: [(Address, String)] -> [(Address, String)]
enumLabel is = map (\((a, l), i) -> (a, l ++ show i)) $ is `zip` [1..]

data Vehicle = Vehicle
  { wheelsIn  :: [DataElem Unqueued Double Required]
  , accelIn   :: DataElem Unqueued Double Required 
  , valvesOut :: [ValveP Provided]
  }

instance External Vehicle where
  fromExternal vh = relabel "ACCEL" (fromExternal (accelIn vh)) ++
                    enumLabel (relabel "WHEEL" (fromExternal (wheelsIn vh)))
  toExternal = toExternal . valvesOut 

-- | Export wheel ports and accelerometer connection from the ABS system.
-- TODO: Export tasks + time delta
vehicleIO :: AUTOSAR Vehicle
vehicleIO = do
  declareTask "core1" (TimingEvent 1e-3)
  absCtrl  <- absSystem
  veloCtrl <- velocityCtrl 1e-2

  connect (velocity veloCtrl) (absVeloIn absCtrl)
  
  let ws = wheels veloCtrl `zip` map veloIn (wheelPorts absCtrl)
  wheelsIn  <- forM ws $ \(vc, wp) -> requiredDelegate [vc, wp]
  valvesOut <- forM (map valveOut (wheelPorts absCtrl)) $ \vp ->
    providedDelegate [vp]
  
  accelIn <- requiredDelegate [accel veloCtrl]

  return $ sealBy Vehicle wheelsIn accelIn valvesOut 

main :: IO ()
main = do
  simulateUsingExternal True vehicleIO
  return ()

