module AUTOSAR.ABS.SimulinkDrive where

import AUTOSAR.ARSim
import AUTOSAR.ABS.ABS
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
--main = simulateDriveExternal "./CarModel" absSystem >> return ()
main = quickCheckWith stdArgs { maxSuccess = 5 } prop_example

prop_example :: () -> Property
prop_example () = monadicIO $ run $ do
  simulateDriveExternal "./CarModel" absSystem
  return True
