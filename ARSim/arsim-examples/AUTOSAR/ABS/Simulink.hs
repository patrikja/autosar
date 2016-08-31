module AUTOSAR.ABS.Simulink where

import AUTOSAR.ARSim
import AUTOSAR.ABS.ABS
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
vehicleIO :: Time -> [Task] -> AUTOSAR Vehicle
vehicleIO delta tasks = composition $ do
  let [veloT, absT, bangT] = tasks

  absCtrl  <- absSystem    delta absT  bangT
  veloCtrl <- velocityCtrl delta veloT

  connect (velocity veloCtrl) (absVeloIn absCtrl)
  
  let ws = wheels veloCtrl `zip` map veloIn (wheelPorts absCtrl)
  wheelsIn  <- forM ws $ \(vc, wp) -> requiredDelegate [vc, wp]
  valvesOut <- forM (map valveOut (wheelPorts absCtrl)) $ \vp ->
    providedDelegate [vp]
  
  accelIn <- requiredDelegate [accel veloCtrl]

  return $ sealBy Vehicle wheelsIn accelIn valvesOut 


-- | Entire system assigned to a single task:
--
--      ORDER    Runnable       Execution policy
--      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
--      0        velocityCtrl   Every cycle, 1 ms 
--      1        mainLoop       (Data reception)
--      2        bangBang       Every cycle, 1 ms
--
--  Sample time set to 1 ms.
tasks1 :: [Task]
tasks1 =
  [ "core1" :-> 0
  , "core1" :-> 1 
  , "core1" :-> 2
  ]

-- | System using the task assignment in 'tasks1'.
singleTaskGood :: AUTOSAR Vehicle
singleTaskGood = do
  let delta = 1e-3
  declareTask "core1" (TimingEvent delta)
  vehicleIO delta tasks1

-- | System using the task assignment in 'tasks1', but with an (obviously) bad
-- triggering event for the task.
singleTaskBad :: AUTOSAR Vehicle
singleTaskBad = do
  let delta = 1e-3
  declareTask "core1" (TimingEvent (1.5 * delta))
  vehicleIO delta tasks1

