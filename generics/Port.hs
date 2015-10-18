-- May be useful: {-# LANGUAGE TypeOperators, DeriveGeneric, DefaultSignatures, DeriveAnyClass, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
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

type Valve = Bool

-- From ../ARSim/NewABS.hs
data ValvePort r c = ValvePort {
        relief   :: DataElement Unqueued Valve r c,
        pressure :: DataElement Unqueued Valve r c  }

instance Port ValvePort where
  type PComSpec ValvePort = (PComSpec (DataElement Unqueued Valve), PComSpec (DataElement Unqueued Valve))
  type RComSpec ValvePort = (RComSpec (DataElement Unqueued Valve), RComSpec (DataElement Unqueued Valve))

{- The type computation for PComSpec and RComSpec is just pushed down to
the record components. A bit annoying that the record type (of
"labelled pairs") becomes just 2-tuple type (of "unlabelled pairs"). -}

{- Haskell also complains that the extension "UndecidableInstances" is
   needed because the type (DataElement Unqueued Valve) is not "seen"
   as being part of the type ValvePort.

   An alternative would be to use a "generic" pairing combinator for
   Ports as in Speculation2.PairPort. This would make the instance
   declaration nicely generic (lifting Port from the component types
   to the record type) but then we'd lose the nice labels also on the
   record type (not only on the PComSpec and RComSpec variants).

   A third possibility would be to introduce a value-level description
   of the record type and use Template Haskell to generate the
   datatype declaration and the instance declarations. Then I think it
   would also be possible to use record types (with labels) for the
   ?ComSpec versions. (But they would need some pre- or suffix to
   avoid name clashing.)

   One could also move from record types altogether and use
   heterogenous lists to represent the labelled (sub-)components in
   Ports.

   In any case, the requirement to write "r c" in suitable places for
   the definition of "Port types" is already a bit confusing.

   A possible syntax (example) could be:

[| port ValvePort = {
        relief   :: DataElement Unqueued Valve,
        pressure :: DataElement Unqueued Valve
        }
|]

-}

{-

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
