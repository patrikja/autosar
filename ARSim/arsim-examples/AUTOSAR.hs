module AUTOSAR
  ( -- * ABS example
    simulinkABS
    -- * ACC example
  , simulinkACCgood1
  , simulinkACCbad1
    -- * Re-export simulation functions
  , simulateUsingExternal, simulateStandalone
  ) where

import qualified AUTOSAR.ABS.Simulink
import qualified AUTOSAR.ACC.Simulink
import           AUTOSAR.ACC.Vehicle
import           AUTOSAR.ARSim


-- TODO: Good/bad for ABS

-- * ABS examples
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- TODO: Do this like ACC
-- | Simulink ABS example, one task, good task mapping.
simulinkABS :: IO ()
simulinkABS = AUTOSAR.ABS.Simulink.main

-- * ACC examples
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Simulink ACC example, one task, good task mapping.
simulinkACCgood1 :: AUTOSAR IOModule
simulinkACCgood1 = AUTOSAR.ACC.Simulink.singleTaskGood

-- | Simulink ACC example, one task, bad task mapping.
simulinkACCbad1 :: AUTOSAR IOModule
simulinkACCbad1 = AUTOSAR.ACC.Simulink.singleTaskBad

