module AUTOSAR
  ( -- * NewABS, NewABS2
    standaloneNewABS
  , simulinkNewABS2
    -- * ACC example
  , simulinkACCgood1
  , simulinkACCbad1
    -- * Re-export simulation functions
  , simulateUsingExternal, simulateStandalone
  ) where

import qualified AUTOSAR.NewABS.Standalone
import qualified AUTOSAR.NewABS2.Simulink
import qualified AUTOSAR.ACC.Simulink
import           AUTOSAR.ACC.Vehicle
import           AUTOSAR.ARSim

-- * ABS examples
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Standalone ABS example. No task mapping.
standaloneNewABS :: IO ()
standaloneNewABS = AUTOSAR.NewABS.Standalone.main

-- | Simulink ABS example, one task, good task mapping.
simulinkNewABS2 :: IO ()
simulinkNewABS2 = AUTOSAR.NewABS2.Simulink.main

-- * ACC examples
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Simulink ACC example, one task, good task mapping.
simulinkACCgood1 :: AUTOSAR IOModule
simulinkACCgood1 = AUTOSAR.ACC.Simulink.singleTaskGood

-- | Simulink ACC example, one task, bad task mapping.
simulinkACCbad1 :: AUTOSAR IOModule
simulinkACCbad1 = AUTOSAR.ACC.Simulink.singleTaskBad

