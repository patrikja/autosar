module Tricky where

import NewARSim

-- This module should not typecheck due to a leaked inter-runnable variable

rmethod rport excl irv = do
        Ok v <- rteRead rport
        rteEnter excl
        Ok s <- rteIrvRead irv
        rteIrvWrite irv (s+v)
        rteExit excl

wmethod pport peek = do
        x <- peek
        rteWrite pport (7+x)

peek irv = do
        Ok v <- rteIrvRead irv
        return v

tricky = do (rp,peek)   <- component $ do rport <- requiredDataElement
                                          excl <- exclusiveArea
                                          irv <- interRunnableVariable (0::Int)
                                          runnable Concurrent [ReceiveE rport] (rmethod rport excl irv)
                                          return (seal rport, peek irv) -- leaks out irv!
            pp          <- component $ do pport <- providedDataElement
                                          runnable Concurrent [Timed 1.0] (wmethod pport peek)
                                          return (seal pport)
            connect pp rp
