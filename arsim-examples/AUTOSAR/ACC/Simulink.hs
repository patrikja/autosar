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
--  0        velocityCtrl   Every cycle, 1 ms 
--  1        mainLoop       (Data reception)
--  2        bangBang       Every cycle, 1 ms
--  3        radarCtrl      Every 10 cycles, 10 ms
--  4        cruiseCtrl     Every 10 cycles, 10 ms
--  5        brakeCtrl      (Data reception)
--  6        throttleCtrl   (Data reception)
--  7        gearController Every 10 cycles, 10 ms
--

-- TODO: 
--   * Export parametrized task assignments, one argument for each runnable in
--     the model.
--   * Export some sort of time step parameter -- Some parts, especially in the
--     ABS system, depend on higher resolutions.
--
-- Ex.
--
-- main :: Task -> Task -> Task -> Task -> Task -> Time -> Time -> IO ()
-- main veloT cruiseT bangT ... = ...
--
-- Following this setup, some good/bad task assignments can be given in the
-- "Examples/" part of the project.

system :: AUTOSAR IOModule
system = do
  let delta  = 1e-3
      veloT  = "core1" :>> (0, 1)
      absT   = "core1" :>> (1, 1)
      bangT  = "core1" :>> (2, 1)
      radarT = "core1" :>> (3, 10) 
      accT   = "core1" :>> (4, 10)
      brkT   = "core1" :>> (5, 10)
      thrT   = "core1" :>> (6, 10)
      gearT  = "core1" :>> (7, 10)

  declareTask "core1" (TimingEvent delta)
  vehicleIO delta [veloT, absT, bangT, radarT, accT, brkT, thrT, gearT] 

main :: IO ()
main = do 
  simulateUsingExternal True system 
  return ()
