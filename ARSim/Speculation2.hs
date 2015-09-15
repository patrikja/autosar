{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, RankNTypes, RecordWildCards #-}
module Speculation2 where 
{-
type A  = DataElementUnqueued Int
type B  = ClientServerOperation (Int,String) Int
type C  = DataElementQueued Double

comp1 :: AR c (RPort () A, RPort () B)
comp1 = atomic $ do
    x <- required UnqueuedReceiverComSpec{init=0}
    m <- required ClientComSpec{}
    runnable [DataReceivedEvent x] $ do
        v <- rte_Read    x
        r <- rte_Call    m (v,"")
        return ()
    return (x, m)

comp2 :: AR c (RPort () A, PPort () B, PPort () C)
comp2 = atomic $ do
    x <- required UnqueuedReceiverComSpec{init=7}
    m <- provided ServerComSpec{length=10}
    z <- provided QueuedSenderComSpec{init=3.14}
    serverRunnable [OperationInvokedEvent m] $ \(i,s) -> do
        v <- rte_Read a x
        _ <- rte_Send b z (v/i)
        return (2*v/i)
    return (x, m, z)
    
comp3 :: AR c (RPort () A, PPort () C)
comp3 = composition $ do
    (a1, b1) <- comp1
    (a2, b2, c2) <- comp2
    connect b1 b2
    a <- delegate [a1, a2]
    return (a, c2)

--------------

runnable        :: [Event           c] ->       RTE c b  -> AR c ()
serverRunnable  :: [ServerEvent a b c] -> (a -> RTE c b) -> AR c ()

-}

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

runnable        :: [Event c]            ->       RTE c b  -> AR c ()
serverRunnable  :: [ ServerEvent a b c] -> (a -> RTE c b) -> AR c ()
runnable = undefined
serverRunnable = undefined

atomic          :: (forall c . AR c (p c)) -> AR c' (p ())
atomic = undefined


data Event c            = DataReceivedEvent (forall q a . DataElement q a Required c)
data ServerEvent a b c  = OperationInvokedEvent (ClientServerOperation a b Provided c)

data AR c a

instance Functor (AR c) where
    fmap = undefined
instance Applicative (AR c) where
    pure = undefined
    (<*>) = undefined
instance Monad (AR c) where
    (>>=) = undefined

data UnqueuedSenderComSpec a    = UnqueuedSenderComSpec { initial :: a }
data UnqueuedReceiverComSpec a  = UnqueuedReceiverComSpec { init :: a }
data QueuedSenderComSpec a      = QueuedSenderComSpec
data QueuedReceiverComSpec a    = QueuedReceiverComSpec { length :: Int }
data ServerComSpec a b
data ClientComSpec              = ClientComSpec

class Interface p where
    seal    :: p r c -> p r ()
    seal = undefined

instance Interface (DataElement q a)
instance Interface (ClientServerOperation a b)

class Interface p => Port p pspec rspec | p -> pspec, p -> rspec, p pspec -> rspec, p rspec -> pspec where
    connect :: p Provided () -> p Required () -> AR c ()
    connect = undefined
    require :: rspec -> AR c (p Required c)
    provide :: pspec -> AR c (p Provided c)
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

data Comp2 r c = Comp2 { port1 :: PP Required c, port2 :: Serv Provided c, port3 :: Dump Provided c}

{-
comp1 :: AR c (Comp1 r ())
comp1 = atomic $ do
    port1 <- require (UnqueuedReceiverComSpec{init=0}, QueuedReceiverComSpec{length=10})
    port2 <- require ClientComSpec
{-    runnable [DataReceivedEvent x] $ do
        v <- rte_Read    x
        r <- rte_Call    m (v,"")
        return ()
-}
    return Comp1 {..}
-}
