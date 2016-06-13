-- | External simulation of the ACC component using Simulink (or similar). 
module Main where

import ACC 
import Control.Monad
import NewARSim 

-- * External simulation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

main :: IO ()
main = simulateUsingExternal cruiseCtrl 
