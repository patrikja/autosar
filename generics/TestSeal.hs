{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
module TestSeal where
import Datatypes
import Seal

deriving instance Generic1  (DataElement q a r)
deriving instance GFunctor  (DataElement q a r)
instance          Interface (DataElement q a r)

t1 :: DataElement q a r ()
t1 = seal (DE 1)

deriving instance GFunctor  (MyPort r)
deriving instance Generic1  (MyPort r)
instance          Interface (MyPort r)  -- rely on the default

t2 :: MyPort r ()
t2 = seal myPort

deriving instance Generic1  (ClientServerOperation a b r)
deriving instance GFunctor  (ClientServerOperation a b r)
instance          Interface (ClientServerOperation a b r)  -- rely on the default

t3 :: Serv r () -- a special case of ClientServerOperation
t3 = seal (OP 3)

deriving instance Generic1  IFace1
deriving instance GFunctor  IFace1
instance          Interface IFace1  -- rely on the default

t4 :: IFace1 ()
t4 = seal iface1

deriving instance Generic1  IFace2
deriving instance GFunctor  IFace2
instance          Interface IFace2  -- rely on the default

t5 :: IFace2 ()
t5 = seal iface2


data IFace3 c   = IFace3 { kalle :: MyPort Required c,
                           eva   :: Serv   Provided c,
                           sven  :: Dump   Provided c }
  deriving (Generic1, GFunctor, Interface, Show)

iFace3 = IFace3 { kalle = myPort,
                  eva   = OP 11,
                  sven  = DE 7
                }

----------------------------------------------------------------
-- Show instances

deriving instance Show (DataElement q a r c)
deriving instance Show (MyPort r c)
deriving instance Show (ClientServerOperation a b r c)
deriving instance Show (IFace1 c)
deriving instance Show (IFace2 c)
