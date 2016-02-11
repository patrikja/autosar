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

data AUTOSAR a

instance Functor AUTOSAR where fmap = undefined
instance Applicative AUTOSAR where pure = undefined; (<*>) = undefined
instance Monad AUTOSAR where (>>=) = undefined

delem :: Atomic c (DataElement c)
delem = undefined
connect :: DataElem -> DataElem -> AUTOSAR ()
connect = undefined

atomic :: (forall c . Atomic c a) -> AUTOSAR a
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
    sealBy :: Unseal a -> a
    sealBy = undefined

instance (Interface a, Sealer b) => Sealer (a -> b) where
    sealBy f a = sealBy (f (seal a))

instance {-# OVERLAPPABLE #-} (Unseal a ~ a) => Sealer a where
    sealBy = id

----------------------

-- MyPort  ::  DataElem -> [DataElem] -> MyPort
--
-- instance Sealer (DataElement c1 -> [DataElement c2] -> MyPort) where
--     sealBy :: Unseal (DataElement c1 -> [DataElement c2] -> MyPort) -> 
--                      (DataElement c1 -> [DataElement c2] -> MyPort)
--             ~ (Seal (DataElement c1) -> Unseal ([DataElement c2] -> Unseal MyPort)) -> 
--                      (DataElement c1 -> [DataElement c2] -> MyPort)
--             ~ (Seal (DataElement c1) -> Seal [DataElement c2] -> Unseal MyPort) -> 
--                      (DataElement c1 -> [DataElement c2] -> MyPort)
--             ~ (DataElem -> [Seal (DataElement c2)] -> MyPort) -> 
--                      (DataElement c1 -> [DataElement c2] -> MyPort)
--             ~ (DataElem -> [DataElem] -> MyPort) -> 
--                      DataElement c1 -> [DataElement c2] -> MyPort


data MyPort = MyPort { e1 :: DataElem,
                       e2 :: [DataElem] }

--comp1 :: t -> AUTOSAR (MyPort, DataElem, Maybe Char)
comp1 x = atomic $ do 
            a <- delem
            b <- delem
            x <- delem
            return $ (sealBy MyPort a [b,x], seal x, Nothing)

comp2 x = do
    (MyPort a (b:_), x, _) <- comp1 ()
    connect a b
    return False

