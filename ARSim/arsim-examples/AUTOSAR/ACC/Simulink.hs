-- | External simulation of the ACC component using Simulink (or similar). 
module AUTOSAR.ACC.Simulink 
  ( module AUTOSAR.ACC.Vehicle
  , singleTaskGood
  , singleTaskBad
  ) where

import AUTOSAR.ACC.Vehicle
import AUTOSAR.ARSim 
import Control.Monad

-- | Entire system assigned to a single task:
--
--      ORDER    Runnable       Execution policy
--      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
--      0        velocityCtrl   Every cycle, 1 ms 
--      1        mainLoop       (Data reception)
--      2        bangBang       Every cycle, 1 ms
--      3        radarCtrl      Every 10 cycles, 10 ms
--      4        cruiseCtrl     Every 10 cycles, 10 ms
--      5        brakeCtrl      (Data reception)
--      6        throttleCtrl   (Data reception)
--      7        gearController Every 10 cycles, 10 ms
--
--  Sample time set to 1 ms.
tasks1 :: [Task]
tasks1 =
  [ "core1" :>> (0, 1)
  , "core1" :>> (1, 1)
  , "core1" :>> (2, 1)
  , "core1" :>> (3, 10)
  , "core1" :>> (4, 10)
  , "core1" :>> (5, 10)
  , "core1" :>> (6, 10)
  , "core1" :>> (7, 10)
  ]

-- | Entire system assigned to a single task with some bad choice. Velocity
-- approximation (or anything else that acts as a source in the data
-- dependencies) appears too late in the task ordering.
--
--      ORDER    Runnable       Execution policy
--      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
--      4        velocityCtrl   Every cycle, 1 ms 
--      0        mainLoop       (Data reception)
--      1        bangBang       Every cycle, 1 ms
--      2        radarCtrl      Every 10 cycles, 10 ms
--      3        cruiseCtrl     Every 10 cycles, 10 ms
--      5        brakeCtrl      (Data reception)
--      6        throttleCtrl   (Data reception)
--      7        gearController Every 10 cycles, 10 ms
--
--  Sample time set to 1 ms.
tasks1bad :: [Task]
tasks1bad =
  [ "core1" :>> (4, 1)
  , "core1" :>> (0, 1)
  , "core1" :>> (1, 1)
  , "core1" :>> (2, 10)
  , "core1" :>> (3, 10)
  , "core1" :>> (5, 10)
  , "core1" :>> (6, 10)
  , "core1" :>> (7, 10)
  ]

-- | System using the task assignment in 'tasks1'.
singleTaskGood :: AUTOSAR IOModule
singleTaskGood = do
  let delta = 1e-3
  declareTask "core1" (TimingEvent delta)
  vehicleIO delta tasks1

-- | System using the task assignment in 'tasks1bad'.
singleTaskBad :: AUTOSAR IOModule
singleTaskBad = do
  let delta = 1e-3
  declareTask "core1" (TimingEvent delta)
  vehicleIO delta tasks1bad

