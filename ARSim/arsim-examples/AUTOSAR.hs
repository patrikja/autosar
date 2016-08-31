module AUTOSAR
  ( -- * ABS example
    simulinkABSgood1
  , simulinkABSbad1
    -- * ACC example
  , simulinkACCgood1
  , simulinkACCbad1
    -- * Re-export simulation functions
  , simulateUsingExternal, simulateStandalone
  ) where

import qualified AUTOSAR.ABS.Simulink
import qualified AUTOSAR.ACC.Simulink
import           AUTOSAR.ABS.Simulink (Vehicle(..))
import           AUTOSAR.ACC.Vehicle
import           AUTOSAR.ARSim

-- * ABS examples
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Simulink ABS example, one task, good task mapping.
simulinkABSgood1 :: AUTOSAR Vehicle
simulinkABSgood1 = AUTOSAR.ABS.Simulink.singleTaskGood

-- | Simulink ABS example, one task, bad task mapping.
simulinkABSbad1 :: AUTOSAR Vehicle
simulinkABSbad1 = AUTOSAR.ABS.Simulink.singleTaskBad

-- * ACC examples
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Simulink ACC example, one task, good task mapping.
simulinkACCgood1 :: AUTOSAR IOModule
simulinkACCgood1 = AUTOSAR.ACC.Simulink.singleTaskGood

-- | Simulink ACC example, one task, bad task mapping.
simulinkACCbad1 :: AUTOSAR IOModule
simulinkACCbad1 = AUTOSAR.ACC.Simulink.singleTaskBad

