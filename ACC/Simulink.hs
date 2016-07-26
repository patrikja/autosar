-- | External simulation of the ACC component using Simulink (or similar). 
module Main where

import Vehicle
import Control.Monad
import NewARSim 

-- * External simulation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 
-- The example is parametrised on the target vehicle parameters. Cruise speed
-- is set externally (by the vehicle model). (Perhaps change this?)
--
-- The vehicle will need some time to accelerate to full speed (i.e. 5 - 10s)
-- depending on cruise setting.

time = 9
dist = 10
velo = 5
dur  = 5

main :: IO ()
main = simulateUsingExternal (vehicleIO time dist velo dur)
