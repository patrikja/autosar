module Datatypes where
type Address                = Int

newtype DataElement q a r c             = DE Address      -- Async channel of "a" data
newtype ClientServerOperation a b r c   = OP Address      -- Sync channel of an "a->b" service

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
data ClientComSpec              = ClientComSpec

data MyPort r c = MyPort { e1 :: DataElement Unqueued Int r c,
                           e2 :: DataElement Queued String r c }

type Serv = ClientServerOperation (Int,String) Bool
  -- Serv r c :: *

data IFace1 c   = IFace1 { portA :: MyPort Required c,
                           portB :: Serv   Required c }

type Dump r c   = DataElement Queued Int r c

data IFace2 c   = IFace2 { port1 :: MyPort Required c,
                           port2 :: Serv   Provided c,
                           port3 :: Dump   Provided c }

----------------
-- Example values

dataElem = DE
myPort   = MyPort {e1 = dataElem 1, e2 = dataElem 2}
csOp     = OP
iface1   = IFace1 {portA = myPort, portB = OP 3}
iface2   = IFace2 {port1 = myPort, port2 = OP 6, port3 = dataElem 7}
