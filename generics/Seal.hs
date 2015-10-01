{-# LANGUAGE TypeOperators, DeriveGeneric, DefaultSignatures, DeriveAnyClass, FlexibleContexts #-}
module Seal (module Seal, module GHC.Generics, module Generics.Deriving.Functor) where
import GHC.Generics
import Generics.Deriving.Functor

class GFunctor m => Interface m where
  seal :: m c -> m ()

  seal = gmap (const ())

-- TODO: Perhaps add rewriting pragmas to actually avoid doing anything.
