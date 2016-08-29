module AUTOSAR.NewABS.Standalone where

import AUTOSAR.NewABS.NewABS
import Control.Monad
import Graphics.EasyPlot
import NewARSim

-------------------------------------------------------------------
--  A simulated physical car
-------------------------------------------------------------------
wheel_f :: Index -> Time -> Bool -> Bool -> Velo -> (Accel, Velo)
wheel_f i time pressure relief velo
        | time < 1.0                = veloStep 0          velo
        | i /= 2                    = veloStep (-4.5)     velo
        -- let wheel 2 skid...
        -- We "postulate" a reasonable approximation of the wheel
        -- speed (ignoring the actual valves). Ideally the whole car
        -- dynamics whould be in another module (in Simulink).
        | time < 1.6                = veloStep (-10)      velo
        | time < 2                  = veloStep (-4)       velo
        | time < 2.5                = veloStep (-3)       velo
        | time < 3                  = veloStep 0.0        velo
        | time < 3.4                = veloStep (-4.5)     velo
        | time < 4                  = veloStep (-5)       velo
        | time < 4.3                = veloStep (-8.4)     velo
        | time < 4.7                = veloStep (-4)       velo
        | otherwise                 = veloStep 0          velo
-- The "pressure logic" is something like this, but a proper
-- treatment need integration over time and knowledge of vehicle speed
-- and other physical parameters.
--        | pressure && not relief    = veloStep (-10)      velo
--        | relief && not pressure    = veloStep (-1)       velo
--        | otherwise                 = veloStep (-3)       velo

timeStep :: Time
timeStep = 0.01

veloStep :: Accel -> Velo -> (Accel, Velo)
veloStep a v = (a, v + a*timeStep)

data Car = Car {
            actuators  :: [ValveP Required],
            v_sensors  :: [DataElem Unqueued Velo  Provided],
            a_sensors  :: [DataElem Unqueued Accel Provided] }

simulated_car :: AUTOSAR Car
simulated_car = atomic $ do
        wheels <- forM [1..4] $ \i -> do
            actuator <- requiredPort
            v_sensor <- providedPort
            a_sensor <- providedPort
            comSpec actuator (InitValue (False, True))
            comSpec v_sensor (InitValue init_v)
            comSpec a_sensor (InitValue init_a)
            when (i<=2) $ do
                probeWrite ("wheel "++show i++" speed")         v_sensor
                probeWrite ("wheel "++show i++" acceleration")  a_sensor
            return (actuator, v_sensor, a_sensor)
        irv <- interRunnableVariable (init_a, replicate 4 init_v)
        runnable (MinInterval 0) [TimingEvent timeStep] $ do
            Ok (time, velos) <- rteIrvRead irv
            let time' = time + timeStep
            velos' <- forM (zip3 [1..] wheels velos) $ \(i, (actuator,v_sensor,a_sensor), velo) -> do
                Ok pressure <- rteRead (pressure actuator) -- TODO: perhaps add error handling
                Ok relief   <- rteRead (relief actuator)   --   in case the return code is not "Ok v"
                let (acc, velo') = wheel_f i time' pressure relief velo
                rteWrite v_sensor velo'
                rteWrite a_sensor acc
                return velo'
            rteIrvWrite irv (time', velos')
            return ()
        let (actuators, v_sensors, a_sensors) = unzip3 wheels
        return $ sealBy Car actuators v_sensors a_sensors

init_v :: Velo
init_v = 18

init_a :: Accel
init_a = 0

-------------------------------------------------------------------------
-- A test setup consists of the ABS implementation and the simulated car
-- cyclically connected into a closed system.
--------------------------------------------------------------------------

test :: AUTOSAR ()
test = do
        w_ports <- abs_system
        car <- simulated_car
        connectEach (v_sensors car)         (map velo_in w_ports)
        connectEach (a_sensors car)         (map accel_in w_ports)
        connectEach (map valve_out w_ports) (actuators car)

makePlot :: Trace -> IO Bool
makePlot trace = plot (PDF "plot.pdf") curves
  where curves  = [ Data2D [Title str, Style Lines, Color (color str)] [] (discrete pts)
                  | (str,pts) <- ms ]
        color "pressure 2"              = Red
        color "relief 2"                = Blue
        color "wheel 2 speed"           = Green
        color "wheel 2 acceleration"    = Violet
        color _                         = Black
        ms                              = bools ++ doubles
        doubles                         = probeAll trace
        bools                           = map scale (probeAll trace)

discrete :: Fractional t => [((q, t), k)] -> [(t, k)]
discrete []                     = []
discrete (((_,t),v):vs)         = (t,v) : disc v vs
  where disc v0 (((_,t),v):vs)  = (t,v0) : (t+eps,v) : disc v vs
        disc _ _                = []
        eps                     = 0.0001

scale :: (ProbeID, Measurement Bool) -> (ProbeID, Measurement Double)
scale ("relief 2", m)   = ("relief 2", map (fmap scaleValve) m)
  where scaleValve      = (+2.0) . fromIntegral . fromEnum
scale ("pressure 2", m) = ("pressure 2", map (fmap scaleValve) m)
  where scaleValve      = (+4.0) . fromIntegral . fromEnum

main :: IO ()
main = 
  do simulateStandalone False 5.0 output (RandomSched (mkStdGen 112)) test
     return ()
  where output trace = printAll trace >> makePlot trace

