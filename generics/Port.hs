-- {-# LANGUAGE TypeOperators, DeriveGeneric, DefaultSignatures, DeriveAnyClass, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Port where

import Datatypes

data AR c a -- Stub

-- From ../ARSim/NewARSim.hs
class Port p where
    type PComSpec p :: *
    type RComSpec p :: *
    connect  :: p Provided () -> p Required () -> AR c ()
    delegateP :: [p Provided ()] -> AR c (p Provided ())
    delegateR :: [p Required ()] -> AR c (p Required ())
    provide  :: PComSpec p -> AR c (p Provided c)
    require  :: RComSpec p -> AR c (p Required c)

-- From ../ARSim/NewABS.hs
data ValvePort r c = ValvePort {
        relief   :: DataElement Unqueued Valve r c,
        pressure :: DataElement Unqueued Valve r c  }

{-

instance Port ValvePort where
        type PComSpec ValvePort = (UnqueuedSenderComSpec Valve, UnqueuedSenderComSpec Valve)
        type RComSpec ValvePort = (UnqueuedReceiverComSpec Valve, UnqueuedReceiverComSpec Valve)
        provide (a,b) = do
            relief <- provide a
            pressure <- provide b
            return ValvePort{..}
        require (a,b) = do
            relief <- require a
            pressure <- require b
            return ValvePort{..}
        connect a b = do
            connect (relief a) (relief b)
            connect (pressure a) (pressure b)
        delegateP ps = do
            relief <- delegateP [ v | ValvePort{ relief = v } <- ps ]
            pressure <- delegateP [ v | ValvePort{ pressure = v } <- ps ]
            return ValvePort{..}
        delegateR ps = do
            relief <- delegateR [ v | ValvePort{ relief = v } <- ps ]
            pressure <- delegateR [ v | ValvePort{ pressure = v } <- ps ]
            return ValvePort{..}
-}
