-- | External simulation of the ACC component using Simulink (or similar). 
module AUTOSAR.ACC.Simulink where

import AUTOSAR.ACC.Vehicle
import Control.Monad
import NewARSim 

-- * External simulation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Current task assignment:
--
--  TASK "core1", 1 ms
--  ORDER    Runnable       Execution policy
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
--  0        velocityCtrl   Every 10 cycles, 10 ms 
--  1        mainLoop       (Data reception)
--  2        bangBang       Every cycle, 1 ms
--  3        radarCtrl      Every 10 cycles, 10 ms
--  4        cruiseCtrl     Every 10 cycles, 10 ms
--  5        brakeCtrl      (Data reception)
--  6        throttleCtrl   (Data reception)
--  7        gearController Every 10 cycles, 10 ms
--

-- TODO: Perhaps we should be able to set task assignments from here,
-- and we could keep some copies of this file instead?

system :: AUTOSAR IOModule
system = do
  declareTask "core1" (TimingEvent 1e-3)
  vehicleIO 

main :: IO ()
main = do 
  simulateUsingExternal True system 
  return ()
