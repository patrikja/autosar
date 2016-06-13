module Plot 
  ( makePlot
  , makePlotX11
  ) where

import Graphics.EasyPlot
import NewARSim

-- | Generate PDF plot.
makePlot :: Trace -> IO Bool
makePlot trace = undefined -- plot (PDF "plot.pdf") curves
  where
    curves = undefined

-- | Generate X11 plot. 
makePlotX11 :: Trace -> IO Bool
makePlotX11 trace = undefined -- plot X11 curves
  where
    curves = undefined
