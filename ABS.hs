module ABS where
        
import ARSim
import System.Random

data ReliefState =
        Stopped |
        Open Int Int |
        Closed Int Int

instance Valuable ReliefState where
        toVal Stopped           = Void
        toVal (Open c l)        = VArray [VBool True, VInt c, VInt l]
        toVal (Closed c l)      = VArray [VBool False, VInt c, VInt l]
        fromVal Void            = Stopped
        fromVal (VArray [VBool True, VInt c, VInt l])
                                = Open c l
        fromVal (VArray [VBool False, VInt c, VInt l])
                                = Closed c l
        
