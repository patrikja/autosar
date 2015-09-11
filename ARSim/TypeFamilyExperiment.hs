{-# LANGUAGE KindSignatures, TypeFamilies #-}
data ReceiverComSpec a
data ClientComSpec i o
data DataElement a
data ClientServerOperation i o

type family MkElOp a
type instance MkElOp (ReceiverComSpec a) = DataElement a
type instance MkElOp (ClientComSpec i o) = ClientServerOperation i o

----------------

type family MapTuple (f :: * -> *) a
type instance MapTuple f (a, b) = (f a, f b)
type instance MapTuple f (a, b, c) = (f a, f b, f c)

type family MapTuple2  a   -- class Interface?
type instance MapTuple2 (a, b)    = (MkElOp a, MkElOp b)
type instance MapTuple2 (a, b, c) = (MkElOp a, MkElOp b, MkElOp c)






-- class Port a where
--   type Elements a
