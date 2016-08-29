-- Placeholder, not sure to export here.
--
-- Some good & bad examples maybe?
module AUTOSAR
  ( -- * NewABS, NewABS2
    standaloneNewABS
  , simulinkNewABS2
    -- * ACC example
  , simulinkACC
  ) where

import qualified AUTOSAR.NewABS.Standalone
import qualified AUTOSAR.NewABS2.Simulink
import qualified AUTOSAR.ACC.Simulink

-- | Standalone ABS example.
standaloneNewABS :: IO ()
standaloneNewABS = AUTOSAR.NewABS.Standalone.main

-- | Simulink ABS example.
simulinkNewABS2 :: IO ()
simulinkNewABS2 = AUTOSAR.NewABS2.Simulink.main

-- | Simulink ACC example.
simulinkACC :: IO ()
simulinkACC = AUTOSAR.ACC.Simulink.main
