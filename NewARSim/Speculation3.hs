{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, RecordWildCards #-}

type Sealed = ()
data DataElement c

class Interface a where
  type Seal a
  seal :: a -> Seal a

instance Interface (DataElement c) where
  type Seal (DataElement c) = DataElement Sealed
  seal = undefined  -- coerce

instance (Interface a, C b) => C (a -> b) where
  type T (a->b) = Seal a -> T b
  wrap f a = wrap (f (seal a))

data Queued         -- Parameter q above
data Unqueued

type DEUI = DataElement Sealed
type DEQS = DataElement Sealed
data MyPort r = MyPort { e1' :: DEUI,
                         e2' :: DEQS}

class C a where
  type T a
  wrap :: T a -> a

-- instance Eq a => C a where
instance C Bool where
  type T Bool = Bool
  wrap = id

instance C (a,b) where
  type T (a,b) = (a, b)
  wrap = id

instance C (MyPort r) where
  type T (MyPort r) = MyPort r
  wrap = id

test1 :: (DEUI, DEQS)
test1 = wrap (,) e1 e2

test2 :: MyPort r
test2 = wrap MyPort e1 e2

e1 :: DataElement c1
e1 = undefined

e2 :: DataElement c2
e2 = undefined




{-
type Serv r c   = ClientServerOperation (Int,String) Bool r c

data IFace1 c   = IFace1 { portA :: MyPort Required c,
                           portB :: Serv   Required c }
-}
