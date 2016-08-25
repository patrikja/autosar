module Main where

import NewARSim
import NewABS2
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
