{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, RankNTypes #-}
{-# LANGUAGE RecordWildCards, ExistentialQuantification #-}
module Speculation2 where 


newtype DataElement q a r c             = El Int
newtype ClientServerOperation a b r c   = Op Int

data Provided
data Required
data Queued
data Unqueued

data RTE c a

instance Functor (RTE c) where
    fmap = undefined
instance Applicative (RTE c) where
    pure = undefined
    (<*>) = undefined
instance Monad (RTE c) where
    (>>=) = undefined

rte_Read    :: DataElement Unqueued a Required c -> RTE c a
rte_Receive :: DataElement Queued a Required c -> RTE c a
rte_Write   :: DataElement Unqueued a Provided c -> a -> RTE c ()
rte_Send    :: DataElement Queued a Provided c -> a -> RTE c ()
rte_Call    :: ClientServerOperation a b Required c -> a -> RTE c b
rte_Read    = undefined
rte_Receive = undefined
rte_Write   = undefined
rte_Send    = undefined
rte_Call    = undefined

runnable        :: [Event c]            ->       RTE c b  -> Atomic c ()
serverRunnable  :: [ ServerEvent a b c] -> (a -> RTE c b) -> Atomic c ()
runnable = undefined
serverRunnable = undefined

atomic          :: (forall c . Atomic c (p c)) -> AR (p ())
atomic = undefined

composition     :: AR a -> AR a
composition = undefined

data Event c            = forall q a . DataReceivedEvent (DataElement q a Required c)
data ServerEvent a b c  = OperationInvokedEvent (ClientServerOperation a b Provided c)

data Atomic c a

instance Functor (Atomic c) where
    fmap = undefined
instance Applicative (Atomic c) where
    pure = undefined
    (<*>) = undefined
instance Monad (Atomic c) where
    (>>=) = undefined

data AR a

instance Functor AR where
    fmap = undefined
instance Applicative AR where
    pure = undefined
    (<*>) = undefined
instance Monad AR where
    (>>=) = undefined

data UnqueuedSenderComSpec a    = UnqueuedSenderComSpec { initial :: a }
data UnqueuedReceiverComSpec a  = UnqueuedReceiverComSpec { init :: a }
data QueuedSenderComSpec a      = QueuedSenderComSpec
data QueuedReceiverComSpec a    = QueuedReceiverComSpec { length :: Int }
data ServerComSpec a b          = ServerComSpec { len :: Int }
data ClientComSpec              = ClientComSpec

class Interface p where
    seal    :: p r c -> p r ()
    seal = undefined

instance Interface (DataElement q a)
instance Interface (ClientServerOperation a b)

class Interface p => Port p pspec rspec | p -> pspec, p -> rspec, p pspec -> rspec, p rspec -> pspec where
    connect :: p Provided () -> p Required () -> AR ()
    delegate :: [p r ()] -> AR (p r ())
    connect = undefined
    delegate = undefined
    require :: rspec -> Atomic c (p Required c)
    provide :: pspec -> Atomic c (p Provided c)
    require = undefined
    provide = undefined

instance Port (DataElement Unqueued a) (UnqueuedSenderComSpec a) (UnqueuedReceiverComSpec a)
instance Port (DataElement Queued a) (QueuedSenderComSpec a) (QueuedReceiverComSpec a)
instance Port (ClientServerOperation a b) (ServerComSpec a b) ClientComSpec

---

data PP r c = PP { e1 :: DataElement Unqueued Int r c, e2 :: DataElement Queued String r c }

instance Interface PP where
    seal p = PP { e1 = seal (e1 p), e2 = seal (e2 p) }

instance Port PP (UnqueuedSenderComSpec Int, QueuedSenderComSpec String) 
                   (UnqueuedReceiverComSpec Int, QueuedReceiverComSpec String) where
    connect a b = do connect (e1 a) (e1 b); connect (e2 a) (e2 b)
    require (spec1,spec2) = do e1 <- require spec1; e2 <- require spec2; return PP {..}
    provide (spec1,spec2) = do e1 <- provide spec1; e2 <- provide spec2; return PP {..}

type Serv r c = ClientServerOperation (Int,String) Bool r c
type Dump r c = DataElement Queued Int r c

data Comp1 r c = Comp1 { portA :: PP Required c, portB :: Serv Required c }

instance Interface Comp1 where
    seal x = Comp1 { portA = seal (portA x), portB = seal (portB x) }

data Comp2 r c = Comp2 { port1 :: PP Required c, port2 :: Serv Provided c, port3 :: Dump Provided c}

instance Interface Comp2 where
    seal x = Comp2 { port1 = seal (port1 x), port2 = seal (port2 x), port3 = seal (port3 x) }


comp1 :: AR (Comp1 r ())
comp1 = atomic $ do
    portA <- require (UnqueuedReceiverComSpec{init=0}, QueuedReceiverComSpec{length=10})
    portB <- require ClientComSpec
    runnable [DataReceivedEvent (e1 portA)] $ do
        v <- rte_Read (e1 portA)
        r <- rte_Call portB (23, "hello")
        return ()
    return Comp1 {..}

comp2 :: AR (Comp2 r ())
comp2 = atomic $ do
    port1 <- require (UnqueuedReceiverComSpec{init=0}, QueuedReceiverComSpec{length=10})
    port2 <- provide ServerComSpec{len=10}
    port3 <- provide QueuedSenderComSpec
    serverRunnable [OperationInvokedEvent port2] $ \(i,s) -> do
        v <- rte_Read (e1 port1)
        _ <- rte_Send port3 i
        return (i > 0)
    return Comp2 {..}

data Comp3 r c = Comp3 { inport :: PP Required c, outport :: Dump Provided c }

instance Interface Comp3 where
    seal x = Comp3 { inport = seal (inport x), outport = seal (outport x) }

comp3 :: AR (Comp3 r ())
comp3 = composition $ do
    c1 <- comp1
    c2 <- comp2
    connect (port2 c2) (portB c1)
    inport <- delegate [portA c1, port1 c2]
    return Comp3 { inport = inport, outport = port3 c2 }