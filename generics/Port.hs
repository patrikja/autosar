-- {-# LANGUAGE TypeOperators, DeriveGeneric, DefaultSignatures, DeriveAnyClass, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Port where

import Datatypes

data AR c a -- Stub

class Port p where -- From ../ARSim/NewARSim.hs
    type PComSpec p :: *
    type RComSpec p :: *
    connect  :: p Provided () -> p Required () -> AR c ()
    delegateP :: [p Provided ()] -> AR c (p Provided ())
    delegateR :: [p Required ()] -> AR c (p Required ())
    provide  :: PComSpec p -> AR c (p Provided c)
    require  :: RComSpec p -> AR c (p Required c)
