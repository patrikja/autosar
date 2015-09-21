{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Speculation2 where
import Control.Monad (liftM2)

data DataElement q a r c            -- Async channel of "a" data
data ClientServerOperation a b r c  -- Sync channel of an "a->b" service

data Queued         -- Parameter q above
data Unqueued

data Required       -- Parameter r above
data Provided

data RTE c a        -- Monad of executable code
data Atomic c a     -- Monad of atomic component building blocks
data Comp a         -- Monad of component combinators

type Sealed = ()
type Component i    = Comp (i Sealed)

-- Primitive operations

rte_Read        :: DataElement Unqueued  a   Required c ->       RTE c a
rte_Write       :: DataElement Unqueued  a   Provided c ->  a -> RTE c ()
rte_Receive     :: DataElement Queued    a   Required c ->       RTE c a
rte_Send        :: DataElement Queued    a   Provided c ->  a -> RTE c ()
rte_Call        :: ClientServerOperation a b Required c ->  a -> RTE c b

runnable        :: [Event c]           ->       RTE c b  -> Atomic c ()
serverRunnable  :: [ServerEvent a b c] -> (a -> RTE c b) -> Atomic c ()

data Event c            = InitEvent
                        | TimingEvent Double
                        | forall q a . DataReceivedEvent (DataElement q a Required c)
data ServerEvent a b c  = OperationInvokedEvent (ClientServerOperation a b Provided c)

atomic          :: Interface i => (forall c . Atomic c (i c)) -> Comp (i ())
composition     :: Comp a -> Comp a

-- Interfaces and ports

class Interface i where
    seal    :: i c -> i Sealed
    seal    = undefined

instance Interface (DataElement q a r)
instance Interface (ClientServerOperation a b r)

-- class Port p pspec rspec | p -> pspec, p -> rspec where
class Port p where
    type PSpec p
    type RSpec p
    connect  :: p Provided () -> p Required () -> Comp ()
    delegate :: [p r ()] -> Comp (p r ())
    require  :: RSpec p -> Atomic c (p Required c)
    provide  :: PSpec p -> Atomic c (p Provided c)

    connect  = undefined
    delegate = undefined
    require  = undefined
    provide  = undefined

instance Port (DataElement Unqueued a) where
  type PSpec  (DataElement Unqueued a) = UnqueuedSenderComSpec a
  type RSpec  (DataElement Unqueued a) = UnqueuedReceiverComSpec a

instance Port (DataElement Queued   a) where
  type PSpec  (DataElement Queued   a) = QueuedSenderComSpec   a
  type RSpec  (DataElement Queued   a) = QueuedReceiverComSpec   a

instance Port (ClientServerOperation a b) where
  type PSpec  (ClientServerOperation a b) = ServerComSpec a b
  type RSpec  (ClientServerOperation a b) = ClientComSpec

data UnqueuedSenderComSpec a    = UnqueuedSenderComSpec { initial :: a }
data UnqueuedReceiverComSpec a  = UnqueuedReceiverComSpec { init :: a }
data QueuedSenderComSpec a      = QueuedSenderComSpec
data QueuedReceiverComSpec a    = QueuedReceiverComSpec { length :: Int }
data ServerComSpec a b          = ServerComSpec { len :: Int }
data ClientComSpec              = ClientComSpec

-- "Implementations"

instance Functor (RTE c) where          fmap = undefined
instance Applicative (RTE c) where      pure = undefined; (<*>) = undefined
instance Monad (RTE c) where            (>>=) = undefined

instance Functor (Atomic c) where       fmap = undefined
instance Applicative (Atomic c) where   pure = undefined; (<*>) = undefined
instance Monad (Atomic c) where         (>>=) = undefined

instance Functor Comp where        fmap = undefined
instance Applicative Comp where    pure = undefined; (<*>) = undefined
instance Monad Comp where          (>>=) = undefined

rte_Read        = undefined
rte_Receive     = undefined
rte_Write       = undefined
rte_Send        = undefined
rte_Call        = undefined

runnable        = undefined
serverRunnable  = undefined
atomic x        = toComp (fmap seal x)
  where toComp  :: Atomic c a -> Comp a
        toComp  = undefined
composition     = undefined


-- ==================================
--             User code
-- ==================================

data MyPort r c = MyPort { e1 :: DataElement Unqueued Int r c,
                           e2 :: DataElement Queued String r c }

type Serv r c   = ClientServerOperation (Int,String) Bool r c

data IFace1 c   = IFace1 { portA :: MyPort Required c,
                           portB :: Serv   Required c }

type Dump r c   = DataElement Queued Int r c

data IFace2 c   = IFace2 { port1 :: MyPort Required c,
                           port2 :: Serv   Provided c,
                           port3 :: Dump   Provided c }


comp1 :: Component IFace1
comp1 = atomic $ do
    portA <- require (UnqueuedReceiverComSpec{init=0}, QueuedReceiverComSpec{length=10})
    portB <- require ClientComSpec
    runnable [DataReceivedEvent (e1 portA)] $ do
        v <- rte_Read (e1 portA)
        r <- rte_Call portB (23, "hello")
        return ()
    return IFace1 {..}

comp2 :: Component IFace2
comp2 = atomic $ do
    port1 <- require (UnqueuedReceiverComSpec{init=0}, QueuedReceiverComSpec{length=10})
    port2 <- provide ServerComSpec{len=10}
    port3 <- provide QueuedSenderComSpec
    serverRunnable [OperationInvokedEvent port2] $ \(i,s) -> do
        v <- rte_Read (e1 port1)
        _ <- rte_Send port3 i
        return (i > 0)
    return IFace2 {..}

data IFace3 c   = IFace3 { inport :: MyPort Required c,
                           outport :: Dump Provided c }

comp3 :: Component IFace3
comp3 = composition $ do
    c1 <- comp1
    c2 <- comp2
    connect (port2 c2) (portB c1)
    inport <- delegate [portA c1, port1 c2]
    return IFace3 { inport = inport, outport = port3 c2 }


-- The necessary boring stuff

-- TODO generate in the style of (newtype) deriving

instance Interface (MyPort r) where
    seal p = MyPort { e1 = seal (e1 p), e2 = seal (e2 p) }

instance Port MyPort where
    type PSpec MyPort = (UnqueuedSenderComSpec   Int, QueuedSenderComSpec   String)
    type RSpec MyPort = (UnqueuedReceiverComSpec Int, QueuedReceiverComSpec String)
    connect = connectMyPort
    require = requireMyPort
    provide = provideMyPort
    -- delegare = ?

connectMyPort :: MyPort Provided () -> MyPort Required () -> Comp ()
connectMyPort a b = do connect (e1 a) (e1 b); connect (e2 a) (e2 b)

requireMyPort :: RSpec MyPort -> Atomic c (MyPort Required c)
provideMyPort :: PSpec MyPort -> Atomic c (MyPort Provided c)
requireMyPort (spec1,spec2) = do e1 <- require spec1; e2 <- require spec2; return MyPort {..}
provideMyPort (spec1,spec2) = do e1 <- provide spec1; e2 <- provide spec2; return MyPort {..}

instance Interface IFace1 where
    seal i = IFace1 { portA = seal (portA i), portB = seal (portB i) }
instance Interface IFace2 where
    seal i = IFace2 { port1 = seal (port1 i), port2 = seal (port2 i), port3 = seal (port3 i) }
instance Interface IFace3 where
    seal i = IFace3 { inport = seal (inport i), outport = seal (outport i) }


----------------
-- A "generic" pairing combinator for Ports

data PairPort f1 f2 r c = PairPort { proj1 :: f1 r c, proj2 :: f2 r c }

instance (Port f1, Port f2) => Port (PairPort f1 f2) where
  type RSpec (PairPort f1 f2) = (RSpec f1, RSpec f2)
  type PSpec (PairPort f1 f2) = (PSpec f1, PSpec f2)
  connect = connectPairPort connect connect
  require = workInsidePairPort require require
  provide = workInsidePairPort provide provide

-- Perhaps a bit too general ...
--   could set r = r1 and c = c1
connectPairPort :: Monad m =>
     (f1 r c -> f3 r1 c1 -> m a)
  -> (f2 r c -> f4 r1 c1 -> m b)
  -> PairPort f1 f2 r c
  -> PairPort f3 f4 r1 c1
  -> m b
connectPairPort c1 c2 a b = do c1 (proj1 a) (proj1 b)
                               c2 (proj2 a) (proj2 b)

workInsidePairPort ::  Monad m =>
     (t1      -> m (f1 r c)) ->
     (    t2  -> m (f2 r c)) ->
     (t1, t2) -> m (PairPort f1 f2 r c)
workInsidePairPort f1 f2 (spec1, spec2) = liftM2 PairPort (f1 spec1) (f2 spec2)
-- = do e1 <- f1 spec1; e2 <- f2 spec2; return $ PairPort e1 e2
