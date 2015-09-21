{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Speculation2 where 

data DataElement q a r c            -- Async channel of "a" data
data ClientServerOperation a b r c  -- Sync channel of an "a->b" service

data Queued         -- Parameter q above
data Unqueued

data Required       -- Parameter r above
data Provided

data RTE c a        -- Monad of executable code
data Atomic c a     -- Monad of atomic component building blocks
data Comp a         -- Monad of component combinators

type Component i    = Comp (i ())

-- Primitive operations

rte_Read        :: DataElement Unqueued a Required c -> RTE c a
rte_Write       :: DataElement Unqueued a Provided c -> a -> RTE c ()
rte_Receive     :: DataElement Queued a Required c -> RTE c a
rte_Send        :: DataElement Queued a Provided c -> a -> RTE c ()
rte_Call        :: ClientServerOperation a b Required c -> a -> RTE c b

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
    seal    :: i c -> i ()
    seal    = undefined

instance Interface (DataElement q a r)
instance Interface (ClientServerOperation a b r)

class Port p where
    type RComSpec p :: *
    type PComSpec p :: *
    connect  :: p Provided () -> p Required () -> Comp ()
    delegate :: [p r ()] -> Comp (p r ())
    require  :: RComSpec p -> Atomic c (p Required c)
    provide  :: PComSpec p -> Atomic c (p Provided c)

    connect  = undefined
    delegate = undefined
    require  = undefined
    provide  = undefined

instance Port (DataElement Unqueued a) where
    type PComSpec (DataElement Unqueued a) = UnqueuedSenderComSpec a
    type RComSpec (DataElement Unqueued a) = UnqueuedReceiverComSpec a

instance Port (DataElement Queued a) where
    type PComSpec (DataElement Queued a) = QueuedSenderComSpec a
    type RComSpec (DataElement Queued a) = QueuedReceiverComSpec a

instance Port (ClientServerOperation a b) where
    type PComSpec (ClientServerOperation a b) = ServerComSpec a b
    type RComSpec (ClientServerOperation a b) = ClientComSpec

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

data IFace3 c   = IFace3 { inport  :: MyPort Required c, 
                           outport :: Dump Provided c }

comp3 :: Component IFace3
comp3 = composition $ do
    c1 <- comp1
    c2 <- comp2
    connect (port2 c2) (portB c1)
    inport <- delegate [portA c1, port1 c2]
    return IFace3 { inport = inport, outport = port3 c2 }
    

-- The necessary boring stuff

instance Interface (MyPort r) where
    seal p = MyPort { e1 = seal (e1 p), e2 = seal (e2 p) }

instance Port MyPort where
    type PComSpec MyPort = (UnqueuedSenderComSpec Int, QueuedSenderComSpec String)
    type RComSpec MyPort = (UnqueuedReceiverComSpec Int, QueuedReceiverComSpec String)
    connect a b = do connect (e1 a) (e1 b); connect (e2 a) (e2 b)
    require (spec1,spec2) = do e1 <- require spec1; e2 <- require spec2; return MyPort {..}
    provide (spec1,spec2) = do e1 <- provide spec1; e2 <- provide spec2; return MyPort {..}

instance Interface IFace1 where
    seal x = IFace1 { portA = seal (portA x), portB = seal (portB x) }

instance Interface IFace2 where
    seal x = IFace2 { port1 = seal (port1 x), port2 = seal (port2 x), port3 = seal (port3 x) }
instance Interface IFace3 where
    seal x = IFace3 { inport = seal (inport x), outport = seal (outport x) }
