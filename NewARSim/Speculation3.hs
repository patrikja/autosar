{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Speculation3 where
import Unsafe.Coerce

newtype DataElement c = DE' Int
newtype DataElem      = DE  Int

data Atomic c a

instance Functor (Atomic c) where fmap = undefined
instance Applicative (Atomic c) where pure = undefined; (<*>) = undefined
instance Monad (Atomic c) where (>>=) = undefined

data Comp a

instance Functor Comp where fmap = undefined
instance Applicative Comp where pure = undefined; (<*>) = undefined
instance Monad Comp where (>>=) = undefined

delem :: Atomic c (DataElement c)
delem = undefined
connect :: DataElem -> DataElem -> Comp ()
connect = undefined

atomic :: (forall c . Atomic c a) -> Comp a
atomic = undefined

--------------------

type family Seal a where
    Seal (DataElement c) = DataElem
    Seal [a]             = [Seal a]
    Seal (a,b)           = (Seal a, Seal b)
    Seal (m a)           = m (Seal a)
    Seal a               = a

class Interface a where
    seal :: a -> Seal a
    seal = Unsafe.Coerce.unsafeCoerce

instance Interface a
    
type family Unseal a where
    Unseal (a->b) = Seal a -> Unseal b
    Unseal a      = a
    
class Sealer a where
    fseal :: Unseal a -> a
    fseal = undefined

instance (Interface a, Sealer b) => Sealer (a -> b) where
    fseal f a = fseal (f (seal a))

instance {-# OVERLAPPABLE #-} (Unseal a ~ a) => Sealer a where
    fseal = id

----------------------

-- MyPort       :: DataElem -> [DataElem] -> MyPort
--
-- instance Sealer (DataElement c1 -> [DataElement c2] -> MyPort) where
--    wrap :: Sealer (DataElement c1 -> [DataElement c2] -> MyPort) -> (DataElement c1 -> [DataElement c2] -> MyPort)
--          ~ (Seal (DataElement c1) -> Seal [DataElement c2] -> MyPort) -> (DataElement c1 -> [DataElement c2] -> MyPort)
--          ~ (DataElem -> [Seal (DataElement c2)] -> MyPort) -> (DataElement c1 -> [DataElement c2] -> MyPort)
--          ~ (DataElem -> [DataElem] -> MyPort) -> DataElement c1 -> [DataElement c2] -> MyPort


data MyPort = MyPort { e1 :: DataElem,
                       e2 :: [DataElem] }
--    deriving Interface

comp1 :: t -> Comp (MyPort, DataElem, Maybe Char)
comp1 x = atomic $ do 
            a <- delem
            b <- delem
            x <- delem
            return $ (fseal MyPort a [b,x], seal x, Nothing)

comp2 x = do
    (MyPort a (b:_), x, _) <- comp1 ()
    connect a b
    return False

