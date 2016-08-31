module AUTOSAR.NewABS2.Simulink where

import AUTOSAR.ARSim
import AUTOSAR.NewABS2.NewABS2
import AUTOSAR.Shared.Velocity
import Control.Monad
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
vehicleIO :: AUTOSAR Vehicle
vehicleIO = do
  let delta = 1e-3
      veloT = "core1" :-> 0
      absT  = "core1" :-> 1
      bangT = "core1" :-> 2
  declareTask "core1" (TimingEvent delta)

  absCtrl  <- absSystem    delta absT  bangT
  veloCtrl <- velocityCtrl delta veloT

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

