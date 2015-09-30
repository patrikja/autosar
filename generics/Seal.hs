{-# LANGUAGE TypeOperators, DeriveGeneric, DefaultSignatures, DeriveAnyClass, FlexibleContexts #-}
module Seal (module Seal, module GHC.Generics) where
import GHC.Generics

{- -- From GHC.Generics:
class Generic1 f where
  -- Representable types of kind * -> *. This class is derivable in GHC with the DeriveGeneric flag on.
  -- Associated Types
  type Rep1 f :: * -> *
  -- Generic representation type
  from1 :: f a -> Rep1 f a  -- Convert from the datatype to its representation
  to1   :: Rep1 f a -> f a  -- Convert from the representation to the datatype

-}

class Interface m where
  seal :: m c -> m ()

  default seal :: (Generic1 m, GInterface (Rep1 m)) => m c -> m ()
  seal = to1 . gseal . from1

-- TODO: an alternative could perhaps be to convince GHC that seal is
-- the identity with an unusual type. This would require unsafeCoerce
-- and would involve the risk that some type ends up in Interface even
-- though it actively uses the parameter c (which should be just a
-- phantom type).

class GInterface f where
  gseal :: f c -> f ()

instance GInterface U1 where
  gseal U1 = U1

instance (GInterface a, GInterface b) => GInterface (a :*: b) where
  gseal (x :*: y) = gseal x :*: gseal y

-- There should not be any sum types in our examples
-- instance (GInterface a, GInterface b) => GInterface (a :+: b) where

instance (GInterface a) => GInterface (M1 i c a) where
  gseal (M1 x) = M1 (gseal x)

-- TODO: Perhaps a bit too polymorphic ...
instance GInterface (K1 i a) where
  gseal (K1 x) = K1 x
