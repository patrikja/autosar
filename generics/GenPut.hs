{-# LANGUAGE TypeOperators, DeriveGeneric, DefaultSignatures, DeriveAnyClass, FlexibleContexts #-}
-- Source:
   -- Section 7.5.4,   "Deriving Typeable instances"
   -- Section 7.6.1.4, "Default method signatures"
   -- Section 7.5.6,   "Deriving any other class"


-- Just a simple example of using GHC.Generics as a warm-up exercise towards the
--   intended use in ../ARSim/
module GenPut (module GenPut, module GHC.Generics) where
import GHC.Generics

{- -- From GHC.Generics:
-- | Unit: used for constructors without arguments
data U1 p = U1

-- | Constants, additional parameters and recursion of kind *
newtype K1 i c p = K1 { unK1 :: c }

-- | Meta-information (constructor names, etc.)
newtype M1 i c f p = M1 { unM1 :: f p }

-- | Sums: encode choice between constructors
infixr 5 :+:
data (:+:) f g p = L1 (f p) | R1 (g p)

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
data (:*:) f g p = f p :*: g p

class Generic a where
  -- Encode the representation of a user datatype
  type Rep a :: * -> *
  -- Convert from the datatype to its representation
  from  :: a -> (Rep a) x
  -- Convert from the representation to the datatype
  to    :: (Rep a) x -> a

class Generic1 f where
  type Rep1 f :: * -> *

  from1  :: f a -> Rep1 f a
  to1    :: Rep1 f a -> f a

-}

data Bin = O | I
  deriving Show

class GSerialize f where
  gput :: f a -> [Bin]

instance GSerialize U1 where
  gput U1 = []

instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
  gput (x :*: y) = gput x ++ gput y

instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
  gput (L1 x) = O : gput x
  gput (R1 x) = I : gput x

instance (GSerialize a) => GSerialize (M1 i c a) where
  gput (M1 x) = gput x

instance (Serialize a) => GSerialize (K1 i a) where
  gput (K1 x) = put x

class Serialize a where
  put :: a -> [Bin]

  default put :: (Generic a, GSerialize (Rep a)) => a -> [Bin]
  put = gput . from
