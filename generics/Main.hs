{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where
import GenPut

type Address                = Int

newtype DataElement q a r c             = DE Address      -- Async channel of "a" data
  deriving (Generic, Serialize)
newtype ClientServerOperation a b r c   = OP Address      -- Sync channel of an "a->b" service
  deriving (Generic, Serialize)

data Queued         -- Parameter q above
data Unqueued

data Required       -- Parameter r above
data Provided

data RTE c a        -- Monad of executable code
data Atomic c a     -- Monad of atomic component building blocks
data Comp a         -- Monad of component combinators

type Sealed = ()
type Component i    = Comp (i Sealed)

data UnqueuedSenderComSpec a    = UnqueuedSenderComSpec { initial :: a }
data UnqueuedReceiverComSpec a  = UnqueuedReceiverComSpec { init :: a }
data QueuedSenderComSpec a      = QueuedSenderComSpec
data QueuedReceiverComSpec a    = QueuedReceiverComSpec { length :: Int }
data ServerComSpec a b          = ServerComSpec { len :: Int }
  deriving (Generic, Serialize)
data ClientComSpec              = ClientComSpec
  deriving (Generic, Serialize)
instance Serialize Int where put = putInt
instance Serialize Bool where put = putBool
-- instance Serialize String where put = error "TBD"

putBool :: Bool -> [Bin]
putBool True = [I]
putBool False = [O]

putInt :: Int -> [Bin]
putInt (-4) = [I,O,O]
putInt (-3) = [I,O,I]
putInt (-2) = [I,I,O]
putInt (-1) = [I,I,I]
putInt   0  = [O,O,O]
putInt   1  = [O,O,I]
putInt   2  = [O,I,O]
putInt   3  = [O,I,I]


data MyPort r c = MyPort { e1 :: DataElement Unqueued Int r c,
                           e2 :: DataElement Queued String r c }
  deriving (Generic, Serialize)

type Serv r c   = ClientServerOperation (Int,String) Bool r c

data IFace1 c   = IFace1 { portA :: MyPort Required c,
                           portB :: Serv   Required c }
  deriving (Generic, Serialize)

type Dump r c   = DataElement Queued Int r c

data IFace2 c   = IFace2 { port1 :: MyPort Required c,
                           port2 :: Serv   Provided c,
                           port3 :: Dump   Provided c }
  deriving (Generic, Serialize)

-- derive instance Generic (UnqueuedSenderComSpec a)

-- instance Serialize a => Serialize (UnqueuedSenderComSpec a)
-- instance Serialize (IFace2 c)

deriving instance Serialize a => Serialize (Maybe a)
deriving instance (Serialize a, Serialize b) => Serialize (a, b)

val1 :: (Maybe (Int, Bool), Bool)
val1 = (Just (3, False), True)

test1 :: [Bin]
test1 = put val1

val2 :: MyPort Int ()
val2 = MyPort (DE 1) (DE 2)

test2 :: [Bin]
test2 = put val2
