{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module NewARSim (module NewARSim, module Data.Dynamic) where
              
import Control.Monad.Operational
import Data.List
import Data.Maybe
import Data.Dynamic hiding (fromDyn)


-- The Code monad -------------------------------------------------------------

type Code a                 = Program RTE a

data RTE a where
    Enter                   :: ExclusiveArea c -> RTE (StdRet ())
    Exit                    :: ExclusiveArea c -> RTE (StdRet ())
    IrvWrite                :: Typeable a => InterRunnableVariable a c -> a -> RTE (StdRet ())
    IrvRead                 :: Typeable a => InterRunnableVariable a c -> RTE (StdRet a)
    Send                    :: Typeable a => ProvidedQueueElement a c -> a -> RTE (StdRet ())
    Receive                 :: Typeable a => RequiredQueueElement a c -> RTE (StdRet a)
    Write                   :: Typeable a => ProvidedDataElement a c -> a -> RTE (StdRet ())
    Read                    :: Typeable a => RequiredDataElement a c -> RTE (StdRet a)
    IsUpdated               :: Typeable a => RequiredDataElement a c -> RTE (StdRet Bool)
    Invalidate              :: Typeable a => ProvidedDataElement a c -> RTE (StdRet ())
    Call                    :: (Typeable a, Typeable b) => RequiredOperation a b c -> a -> RTE (StdRet b)
    CallAsync               :: (Typeable a, Typeable b) => RequiredOperation a b c -> a -> RTE (StdRet ())
    Result                  :: (Typeable a, Typeable b) => RequiredOperation a b c -> RTE (StdRet b)

rte_send     pqe  a  = singleton $ Send pqe a
rte_receive  rqe     = singleton $ Receive rqe

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
                            deriving (Eq)


-- Simulator state ------------------------------------------------------------

data State                  = State {
                                    procs   :: [Proc],
                                    conns   :: [Conn],
                                    context :: Address,
                                    next    :: Int
                                }

data Proc                   = Run       Address Time Act Int Static
                            | RInst     Address (Maybe Client) [Address] (Code Dynamic)
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
                                    implementation  :: Dynamic -> Code Dynamic
                                }        

state0                      = State { procs = [], conns = [], context = [], next = 1 }


-- The AR monad ---------------------------------------------------------------

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


-- Restricting connections ----------------------------------------------------

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



-- Derived AR operations ------------------------------------------------------

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
                                 newProcess (Run a 0.0 act 0 (Static watch inv code'))
  where periods             = [ t | Timed t <- trig ]
        watch               = [ a | ReceiveE (RE a) <- trig ] ++ [ a | ReceiveQ (RQ a) <- trig ]
        act                 = if null [ Init | Init <- trig ] then Idle else Pending
        code'               = \void -> code >> return void

serverRunnable inv ops code = do a <- newAddress
                                 newProcess (Run a 0.0 act 0 (Static watch inv code'))
  where watch               = [ a | PO a <- ops ]
        act                 = Serving [] []
        code'               = fmap toDyn . code . fromDyn

fromDyn                     :: Typeable a => Dynamic -> a
fromDyn                     = fromJust . fromDynamic

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
                            | VETO


may_say (Run a 0.0 Pending n s)
    | n == 0 || invocation s == Concurrent      = NEW   a
may_say (Run a 0.0 (Serving (c:cs) (v:vs)) n s)    
    | n == 0 || invocation s == Concurrent      = NEW   a
may_say (Run a t act n s) | t > 0.0             = DELTA t
may_say (Timer a 0.0 t)                         = TICK  a
may_say (Timer a t t0) | t > 0.0                = DELTA t
may_say (RInst a c ex code)                     = may_say' (view code)
  where may_say' (Enter (EX x) :>>= cont)       = ENTER x
        may_say' (Exit (EX x) :>>= cont)        = case ex of
                                                    y:ys | y==x -> EXIT x
                                                    _           -> VETO
        may_say' (IrvRead (IV s) :>>= cont)     = IRVR  s NO_DATA
        may_say' (IrvWrite (IV s) v :>>= cont)  = IRVW  s (toDyn v)
        may_say' (Receive (RQ e) :>>= cont)     = RCV   e NO_DATA
        may_say' (Send (PQ e) v :>>= cont)      = SND   e (toDyn v) ok
        may_say' (Read (RE e) :>>= cont)        = RD    e NO_DATA
        may_say' (Write (PE e) v :>>= cont)     = WR    e (toDyn v)
        may_say' (IsUpdated (RE e) :>>= cont)   = UP    e NO_DATA
        may_say' (Invalidate (PE e) :>>= cont)  = INV   e
        may_say' (Call (RO o) v :>>= cont)      = CALL  o (toDyn v) NO_DATA
        may_say' (Result (RO o) :>>= cont)      = RES   o NO_DATA
        may_say' (Return v)                     = case c of
                                                    Just b  -> RET  b (toDyn v)
                                                    Nothing -> TERM a
may_say _                                       = VETO   -- most processes can't say anything


say (NEW _)   (Run a _ Pending n s)                     = [Run a (minstart s) Idle (n+1) s,
                                                           RInst a Nothing [] (implementation s (toDyn ()))]
say (NEW _)   (Run a _ (Serving (c:cs) (v:vs)) n s)     = [Run a (minstart s) (Serving cs vs) (n+1) s,
                                                           RInst a (Just c) [] (implementation s v)]
say (DELTA d) (Run a t act n s)                         = [Run a (t-d) act n s]
say (TICK _)  (Timer a _ t)                             = [Timer a t t]
say (DELTA d) (Timer a t t0)                            = [Timer a (t-d) t0]
say label     (RInst a c ex code)                       = say' label (view code)
  where say' (ENTER _)      (Enter (EX x) :>>= cont)    = [RInst a c (x:ex) (cont void)]
        say' (EXIT _)       (Exit (EX x) :>>= cont)     = [RInst a c ex (cont void)]
        say' (IRVR _ res)   (IrvRead _ :>>= cont)       = [RInst a c ex (cont (fromStdDyn res))]
        say' (IRVW _ _)     (IrvWrite _ _ :>>= cont)    = [RInst a c ex (cont void)]
        say' (RCV _ res)    (Receive _ :>>= cont)       = [RInst a c ex (cont (fromStdDyn res))]
        say' (SND _ _ res)  (Send _ _ :>>= cont)        = [RInst a c ex (cont (fromStdDyn res))]
        say' (RD _ res)     (Read _ :>>= cont)          = [RInst a c ex (cont (fromStdDyn res))]
        say' (WR _ _)       (Write _ _ :>>= cont)       = [RInst a c ex (cont void)]
        say' (UP _ res)     (IsUpdated _ :>>= cont)     = [RInst a c ex (cont (fromStdDyn res))]
        say' (INV _)        (Invalidate _ :>>= cont)    = [RInst a c ex (cont void)]
        say' (CALL _ _ res) (Call _ _ :>>= cont)        = [RInst a c ex (cont (fromStdDyn res))]
        say' (RES _ res)    (Result _ :>>= cont)        = [RInst a c ex (cont (fromStdDyn res))]
        say' (RET _ _)      (Return v)                  = [RInst a Nothing ex (return (toDyn ()))]
        say' (TERM _)       (Return _)                  = []

ok              = Ok (toDyn ())

void            = Ok ()

fromStdDyn (Ok v)   = Ok (fromDyn v)
fromStdDyn NO_DATA  = NO_DATA
fromStdDyn LIMIT    = LIMIT

minstart s      = case invocation s of
                    MinInterval t -> t
                    Concurrent    -> 0.0

trig conn a s   = or [ a `conn` b | b <- triggers s ]


may_hear conn VETO _                                            = VETO
may_hear conn (ENTER a)       (Excl b Free)     | a==b          = ENTER a -- 
may_hear conn (ENTER a)       (Excl b _)        | a==b          = VETO
may_hear conn (EXIT a)        (Excl b Taken)    | a==b          = EXIT a
may_hear conn (EXIT a)        (Excl b _)        | a==b          = VETO
may_hear conn (IRVR a _)      (Irv b v)         | a==b          = IRVR a (Ok v)
may_hear conn (IRVW a v)      (Irv b _)         | a==b          = IRVW a v
may_hear conn (RCV a _)       (QElem b n (v:_)) | a==b          = RCV a (Ok v)
may_hear conn (RCV a _)       (QElem b n [])    | a==b          = RCV a NO_DATA
may_hear conn (SND a v res)   (QElem b n vs)
        | a `conn` b && length vs < n                           = SND a v res
        | a `conn` b                                            = SND a v LIMIT
may_hear conn (SND a v res)   (Run _ _ _ _ s)   | trig conn a s = SND a v res
may_hear conn (RD a _)        (DElem b u v)     | a==b          = RD a v
may_hear conn (WR a v)        (DElem b _ _)     | a `conn` b    = WR a v
may_hear conn (WR a v)        (Run _ _ _ _ s)   | trig conn a s = WR a v
may_hear conn (UP a _)        (DElem b u _)     | a==b          = UP a (Ok (toDyn u))
may_hear conn (INV a)         (DElem b _ _)     | a `conn` b    = INV a
may_hear conn (CALL a v res)  (Run b t (Serving cs vs) n s)
        | trig conn a s && a `notElem` cs                       = CALL a v ok
        | trig conn a s                                         = CALL a v LIMIT
may_hear conn (RES a _)       (Op b (v:vs))     | a==b          = RES a (Ok v)
--may_hear conn (RES a Bottom)  (Op b [])         | a==b          = RES a NO_DATA
may_hear conn (RES a _)       (Op b [])         | a==b          = VETO
may_hear conn (RET a v)       (Op b vs)         | a==b          = RET a v
may_hear conn (TERM a)        (Run b _ _ _ _)   | a==b          = TERM a
may_hear conn (TICK a)        (Run b _ _ _ _)   | a==b          = TICK a
may_hear conn (DELTA d)       (Run _ t _ _ _)   | t == 0        = DELTA d
                                                | d <= t        = DELTA d
                                                | d > t         = VETO
may_hear conn (DELTA d)       (Timer _ t _)     | d <= t        = DELTA d
                                                | d > t         = VETO
may_hear conn label _                                           = label


hear conn (ENTER a)     (Excl b Free)      | a==b               = Excl b Taken
hear conn (EXIT a)      (Excl b Taken)     | a==b               = Excl b Free
hear conn (IRVR a _)    (Irv b v)          | a==b               = Irv b v
hear conn (IRVW a v)    (Irv b _)          | a==b               = Irv b v
hear conn (RCV a _)     (QElem b n (v:vs)) | a==b               = QElem b n vs
hear conn (RCV a _)     (QElem b n [])     | a==b               = QElem b n []
hear conn (SND a v _)   (QElem b n vs) 
        | a `conn` b && length vs < n                           = QElem b n (vs++[v])
        | a `conn` b                                            = QElem b n vs
hear conn (SND a _ _)   (Run b t _ n s)    | trig conn a s      = Run b t Pending n s
hear conn (RD a _)      (DElem b _ v)      | a==b               = DElem b False v
hear conn (WR a v)      (DElem b _ _)      | a `conn` b         = DElem b True (Ok v)
hear conn (WR a _)      (Run b t _ n s)    | trig conn a s      = Run b t Pending n s
hear conn (UP a _)      (DElem b u v)      | a==b               = DElem b u v
hear conn (INV a)       (DElem b _ _)      | a `conn` b         = DElem b True NO_DATA
hear conn (CALL a v _)  (Run b t (Serving cs vs) n s)
        | trig conn a s && a `notElem` cs                       = Run b t (Serving (cs++[a]) (vs++[v])) n s
        | trig conn a s                                         = Run b t (Serving cs vs) n s 
hear conn (RES a _)     (Op b (v:vs))         | a==b            = Op b vs
hear conn (RES a _)     (Op b [])             | a==b            = Op b []
hear conn (RET a v)     (Op b vs)             | a==b            = Op b (vs++[v])
hear conn (TERM a)      (Run b t act n s)     | a==b            = Run b t act (n-1) s
hear conn (TICK a)      (Run b t _ n s)       | a==b            = Run b t Pending n s
hear conn (DELTA d)     (Run b 0.0 act n s)                     = Run b 0.0 act n s
hear conn (DELTA d)     (Run b t act n s)                       = Run b (t-d) act n s
hear conn (DELTA d)     (Timer b t t0)                          = Timer b (t-d) t0
hear conn label         proc                                    = proc


step conn procs        = explore conn [] labels procs
  where labels         = map (respond . may_say)      procs
        respond label  = foldl (may_hear conn) label  procs

explore conn pre (VETO:labels) (p:post) = explore conn (p:pre) labels post
explore conn pre (l:labels)    (p:post) = commit : explore conn (p:pre) labels post
  where commit                          = (l, map (hear conn l) pre ++ say l p ++ map (hear conn l) post)
explore conn _ _ _                      = []
