{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module NewARSim where
              
import Control.Monad.Operational
import Data.List
import Data.Maybe
import Data.Dynamic


-- The Code monad --------------------------------------------------------------------------------------

type Code a                 = Program RTE a

data RTE a where
    Send                    :: Typeable a => ProvidedQueueElement a c -> a -> RTE (StdRet ())
    Receive                 :: Typeable a => RequiredQueueElement a c -> RTE (StdRet a)
    Write                   :: Typeable a => ProvidedDataElement a c -> a -> RTE (StdRet ())
    Read                    :: Typeable a => RequiredDataElement a c -> RTE (StdRet a)
    IsUpdated               :: Typeable a => RequiredDataElement a c -> RTE (StdRet Bool)
    Invalidate              :: Typeable a => ProvidedDataElement a c -> RTE (StdRet ())
    Call                    :: (Typeable a, Typeable b) => RequiredOperation a b c -> a -> RTE (StdRet b)
    CallAsync               :: (Typeable a, Typeable b) => RequiredOperation a b c -> a -> RTE (StdRet ())
    Result                  :: (Typeable a, Typeable b) => RequiredOperation a b c -> RTE (StdRet b)
    IrvWrite                :: Typeable a => InterRunnableVariable a c -> a -> RTE (StdRet ())
    IrvRead                 :: Typeable a => InterRunnableVariable a c -> RTE (StdRet a)
    Enter                   :: ExclusiveArea c -> RTE (StdRet ())
    Exit                    :: ExclusiveArea c -> RTE (StdRet ())

data StdRet a               = Ok a
                            | NO_DATA
                            | NEVER_RECEIVED
                            | LIMIT
                            | UNCONNECTED
                            | TIMEOUT
                            | IN_EXCLUSIVE_AREA

newtype RequiredDataElement a c     = RE Address
newtype ProvidedDataElement a c     = PE Address
newtype RequiredQueueElement a c    = RQ Address
newtype ProvidedQueueElement a c    = PQ Address
newtype RequiredOperation a b c     = RO Address
newtype ProvidedOperation a b c     = PO Address
newtype InterRunnableVariable a c   = IV Address
newtype ExclusiveArea c             = EX Address

type Time                   = Double

data Trigger c              = forall a. ReceiveE (RequiredDataElement a c)
                            | forall a. ReceiveQ (RequiredQueueElement a c)
                            | Timed Time
                            | Init

data Invocation             = Concurrent
                            | MinInterval Time


-- Simulator state -----------------------------------------------------------------------------------------------

data State                  = State {
                                    procs   :: [Proc],
                                    conns   :: [Conn],
                                    context :: Address,
                                    next    :: Int
                                }

data Proc                   = Run       Address Time Act Int Static
                            | RInst     Address (Maybe Client) [Address] (Code ())
                            | Excl      Address Exclusive
                            | Irv       Address Dynamic
                            | Timer     Address Time Time
                            | QElem     Address Int [Dynamic]
                            | DElem     Address Bool (StdRet Dynamic)
                            | Op        Address [Dynamic]

type Conn                   = (Address, Address)

type Address                = [Int]

type Client                 = Address

data Act                    = Idle
                            | Pending
                            | Serving [Client] [Dynamic]

data Exclusive              = Free | Taken

data Static                 = Static {
                                    triggers        :: [Address],
                                    invocation      :: Invocation,
                                    implementation  :: Dynamic -> Code ()
                                }        

state0                      = State { procs = [], conns = [], context = [], next = 1 }


-- The AR monad -------------------------------------------------------------------------------------------------

data ARInstr c a where
    NewAddress              :: ARInstr c Address
    NewProcess              :: Proc -> ARInstr c ()
    Component               :: (forall c. AR c a) -> ARInstr c a
    Connect                 :: Connectable a b => a -> b -> ARInstr c ()

type AR c a                 = Program (ARInstr c) a

runAR                       :: (forall c . AR c a) -> State -> (a,State)
runAR sys st                = run sys st
  where
    run                     :: AR c a -> State -> (a,State)
    run sys st              = run' (view sys) st
    run'                    :: ProgramView (ARInstr c) a -> State -> (a,State)
    run' (NewAddress :>>= sys) st
                            = run (sys (next st : context st)) (st { next = next st + 1 })
    run' (NewProcess p :>>= sys) st
                            = run (sys ()) (st { procs = p : procs st })
    run' (Component subsys :>>= sys) st
                            = let (a,st') = runAR subsys (push st) in run (sys a) (pop st')
      where push st         = st { context = next st : context st, next = 1 }
            pop st          = st { context = tail (context st), next = head (context st) + 2 }
    run' (Connect a b :>>= sys) st
                            = run (sys ()) (st { conns = connection a b : conns st })
    run' (Return a) st      = (a,st)


-- Restricting connections -----------------------------------------------------------------------------------

class Connectable a b | a -> b, b -> a where
    connection              :: a -> b -> Conn

instance Connectable (ProvidedDataElement a ()) (RequiredDataElement a ()) where
    connection (PE a) (RE b)    = (a,b)
        
instance Connectable (ProvidedQueueElement a ()) (RequiredQueueElement a ()) where
    connection (PQ a) (RQ b)    = (a,b)
        
instance Connectable (RequiredOperation a b ()) (ProvidedOperation a b ()) where
    connection (RO a) (PO b)    = (a,b)


class Seal m where
        seal                :: m c -> m ()

instance Seal (RequiredDataElement a) where
        seal (RE a)         = RE a

instance Seal (ProvidedDataElement a) where
        seal (PE a)         = PE a

instance Seal (RequiredQueueElement a) where
        seal (RQ a)         = RQ a

instance Seal (ProvidedQueueElement a) where
        seal (PQ a)         = PQ a

instance Seal (RequiredOperation a b) where
        seal (RO a)         = RO a

instance Seal (ProvidedOperation a b) where
        seal (PO a)         = PO a



-- Derived AR operations -----------------------------------------------------------------------------------------

component                   :: (forall c. AR c a) -> AR c a
connect                     :: Connectable a b => a -> b -> AR c ()
requiredDataElement         :: AR c (RequiredDataElement a c)
requiredDataElement1        :: Typeable a => a -> AR c (RequiredDataElement a c)
providedDataElement         :: AR c (ProvidedDataElement a c)
requiredQueueElement        :: Int -> AR c (RequiredQueueElement a c)
providedQueueElement        :: AR c (ProvidedQueueElement a c)
requiredOperation           :: AR c (RequiredOperation a b c)
providedOperation           :: AR c (ProvidedOperation a b c)
interRunnableVariable       :: Typeable a => a -> AR c (InterRunnableVariable a c)
exclusiveArea               :: AR c (ExclusiveArea c)
runnable                    :: Invocation -> [Trigger c] -> Code a -> AR c ()
serverRunnable              :: (Typeable a, Typeable b) => Invocation -> [ProvidedOperation a b c] -> (a -> Code b) -> AR c ()

component c                 = singleton $ Component c
connect a b                 = singleton $ Connect a b

newAddress                  = singleton NewAddress
newProcess p                = singleton $ NewProcess p

requiredDataElement         = do a <- newAddress; newProcess (DElem a False NO_DATA); return (RE a)
requiredDataElement1 val    = do a <- newAddress; newProcess (DElem a False (Ok (toDyn val))); return (RE a)
providedDataElement         = do a <- newAddress; return (PE a)
requiredQueueElement size   = do a <- newAddress; newProcess (QElem a size []); return (RQ a)
providedQueueElement        = do a <- newAddress; return (PQ a)
requiredOperation           = do a <- newAddress; newProcess (Op a []); return (RO a)
providedOperation           = do a <- newAddress; return (PO a)
interRunnableVariable val   = do a <- newAddress; newProcess (Irv a (toDyn val)); return (IV a)
exclusiveArea               = do a <- newAddress; newProcess (Excl a Free); return (EX a)

runnable inv trig code      = do a <- newAddress
                                 mapM (newProcess . Timer a 0.0) periods
                                 newProcess (Run a 0.0 act 0 (Static watch inv (\_ -> code >> return ())))
  where periods             = [ t | Timed t <- trig ]
        watch               = [ a | ReceiveE (RE a) <- trig ] ++ [ a | ReceiveQ (RQ a) <- trig ]
        act                 = if null [ Init | Init <- trig ] then Idle else Pending

serverRunnable inv ops code = do a <- newAddress
                                 newProcess (Run a 0.0 act 0 (Static watch inv code'))
  where watch               = [ a | PO a <- ops ]
        act                 = Serving [] []
        code'               = mkret . code . fromJust . fromDynamic
        mkret instr         = do a <- instr
                                 let x = toDyn a
                                 -- ...
                                 return ()


connectAll a b              = mapM (uncurry connect) (a `zip` b)
        
connect2 (a1,a2) (b1,b2)        
                            = do connect a1 b1; connect a2 b2
connect3 (a1,a2,a3) (b1,b2,b3)  
                            = do connect a1 b1; connect2 (a2,a3) (b2,b3)
connect4 (a1,a2,a3,a4) (b1,b2,b3,b4)  
                            = do connect a1 b1; connect3 (a2,a3,a4) (b2,b3,b4)
connect5 (a1,a2,a3,a4,a5) (b1,b2,b3,b4,b5)
                            = do connect a1 b1; connect4 (a2,a3,a4,a5) (b2,b3,b4,b5)
connect6 (a1,a2,a3,a4,a5,a6) (b1,b2,b3,b4,b5,b6)  
                            = do connect a1 b1; connect5 (a2,a3,a4,a5,a6) (b2,b3,b4,b5,b6)


sealAll xs                  = map seal xs

seal2 (a1,a2)               = (seal a1, seal a2)
seal3 (a1,a2,a3)            = (seal a1, seal a2, seal a3)
seal4 (a1,a2,a3,a4)         = (seal a1, seal a2, seal a3, seal a4)
seal5 (a1,a2,a3,a4,a5)      = (seal a1, seal a2, seal a3, seal a4, seal a5)
seal6 (a1,a2,a3,a4,a5,a6)   = (seal a1, seal a2, seal a3, seal a4, seal a5, seal a6)




data Label                  = ENTER Address
                            | EXIT  Address
                            | IRVR  Address (StdRet Dynamic)
                            | IRVW  Address Dynamic
                            | RCV   Address (StdRet Dynamic)
                            | SND   Address Dynamic(StdRet Dynamic)
                            | RD    Address (StdRet Dynamic)
                            | WR    Address Dynamic
                            | UP    Address (StdRet Dynamic)
                            | INV   Address
                            | CALL  Address Dynamic (StdRet Dynamic)
                            | RES   Address (StdRet Dynamic)
                            | RET   Address Dynamic
                            | NEW   Address
                            | TERM  Address
                            | TICK  Address
                            | DELTA Time
                            | PASS
