{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module NewARSim (module NewARSim, Typeable, Data) where
              
import Control.Monad.Operational
import Control.Monad.Identity hiding (void)
import Control.Monad.State hiding (void)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Dynamics
import System.Random


-- The RTE monad -------------------------------------------------------------

type RTE c a                = Program (RTEop c) a

data RTEop c a where
    Enter                   :: ExclusiveArea c -> RTEop c (StdRet ())
    Exit                    :: ExclusiveArea c -> RTEop c (StdRet ())
    IrvWrite                :: Data a => InterRunnableVariable a c -> a -> RTEop c (StdRet ())
    IrvRead                 :: Data a => InterRunnableVariable a c -> RTEop c (StdRet a)
    Send                    :: Data a => ProvidedQueueElement a c -> a -> RTEop c (StdRet ())
    Receive                 :: Data a => RequiredQueueElement a c -> RTEop c (StdRet a)
    Write                   :: Data a => ProvidedDataElement a c -> a -> RTEop c (StdRet ())
    Read                    :: Data a => RequiredDataElement a c -> RTEop c (StdRet a)
    IsUpdated               :: RequiredDataElement a c -> RTEop c (StdRet Bool)
    Invalidate              :: ProvidedDataElement a c -> RTEop c (StdRet ())
    Call                    :: Data a => RequiredOperation a b c -> a -> RTEop c (StdRet ())
    Result                  :: Data b => RequiredOperation a b c -> RTEop c (StdRet b)
    Printlog                :: String -> RTEop c ()

rteEnter                   :: ExclusiveArea c -> RTE c (StdRet ())
rteExit                    :: ExclusiveArea c -> RTE c (StdRet ())
rteIrvWrite                :: Data a => InterRunnableVariable a c -> a -> RTE c (StdRet ())
rteIrvRead                 :: Data a => InterRunnableVariable a c -> RTE c (StdRet a)
rteSend                    :: Data a => ProvidedQueueElement a c -> a -> RTE c (StdRet ())
rteReceive                 :: Data a => RequiredQueueElement a c -> RTE c (StdRet a)
rteWrite                   :: Data a => ProvidedDataElement a c -> a -> RTE c (StdRet ())
rteRead                    :: Data a => RequiredDataElement a c -> RTE c (StdRet a)
rteIsUpdated               :: RequiredDataElement a c -> RTE c (StdRet Bool)
rteInvalidate              :: ProvidedDataElement a c -> RTE c (StdRet ())
rteCall                    :: (Data a, Data b) => RequiredOperation a b c -> a -> RTE c (StdRet b)
rteCallAsync               :: Data a => RequiredOperation a b c -> a -> RTE c (StdRet ())
rteResult                  :: Data b => RequiredOperation a b c -> RTE c (StdRet b)

printlog                    :: String -> RTE c ()

rteEnter       ex      = singleton $ Enter      ex
rteExit        ex      = singleton $ Exit       ex
rteIrvWrite    irv  a  = singleton $ IrvWrite   irv  a
rteIrvRead     irv     = singleton $ IrvRead    irv
rteSend        pqe  a  = singleton $ Send       pqe  a
rteReceive     rqe     = singleton $ Receive    rqe
rteWrite       pqe  a  = singleton $ Write      pqe  a
rteRead        rde     = singleton $ Read       rde
rteIsUpdated   rde     = singleton $ IsUpdated  rde
rteInvalidate  pde     = singleton $ Invalidate pde
rteCall        rop  a  = rteCallAsync rop a >>= cont
  where cont (Ok ())   = rteResult rop
        cont LIMIT     = return LIMIT
rteCallAsync   rop  a  = singleton $ Call       rop  a
rteResult      rop     = singleton $ Result     rop

printlog str                = singleton $ Printlog str

data StdRet a               = Ok a
                            | NO_DATA
                            | NEVER_RECEIVED
                            | LIMIT
                            | UNCONNECTED
                            | TIMEOUT
                            | IN_EXCLUSIVE_AREA
                            deriving Show

newtype RequiredDataElement a c     = RE Address
newtype ProvidedDataElement a c     = PE Address
newtype RequiredQueueElement a c    = RQ Address
newtype ProvidedQueueElement a c    = PQ Address
newtype RequiredOperation a b c     = RO Address
newtype ProvidedOperation a b c     = PO Address
newtype InterRunnableVariable a c   = IV Address
newtype ExclusiveArea c             = EX Address

type RequiredDataElem a         = RequiredDataElement a ()
type ProvidedDataElem a         = ProvidedDataElement a ()
type RequiredQueueElem a        = RequiredQueueElement a ()
type ProvidedQueueElem a        = ProvidedQueueElement a ()
type RequiredOp a b             = RequiredOperation a b ()
type ProvidedOp a b             = ProvidedOperation a b ()

type Time                   = Double

data Trigger c              = forall a. ReceiveE (RequiredDataElement a c)
                            | forall a. ReceiveQ (RequiredQueueElement a c)
                            | Timed Time
                            | Init

data Invocation             = Concurrent
                            | MinInterval Time
                            deriving (Eq)


-- Simulator state ------------------------------------------------------------

data SimState               = SimState {
                                    procs    :: [Proc],
                                    conns    :: [Conn],
                                    probes   :: [Probe],
                                    initvals :: Map.Map Address Value,
                                    nextA    :: Address
                                }

data Proc                   = forall c . 
                              Run       Address Time Act Int (Static c)
                            | forall c . 
                              RInst     Address (Maybe Client) [Address] (RTE c Value)
                            | Excl      Address Exclusive
                            | Irv       Address Value
                            | Timer     Address Time Time
                            | QElem     Address Int [Value]
                            | DElem     Address Bool (StdRet Value)
                            | Op        Address [Value]

type Conn                   = (Address, Address)

type Probe                  = (String, Label -> Maybe Value)

type Address                = Int

type Client                 = Address

data Act                    = Idle
                            | Pending
                            | Serving [Client] [Value]

data Exclusive              = Free | Taken

data Static c               = Static {
                                    triggers        :: [Address],
                                    invocation      :: Invocation,
                                    implementation  :: Value -> RTE c Value
                                }        

type ConnRel = Address -> Address -> Bool

state0                      = SimState { procs = [], conns = [], probes = [], initvals = Map.empty, nextA = 0 }

apInit conn mp p@(DElem a f NO_DATA)
                            = case [ v | (b,a') <- conn, a'==a, Just v <- [Map.lookup b mp] ] of
                                [v] -> DElem a f (Ok v)
                                _   -> p
apInit conn mp p            = p

-- The AR monad ---------------------------------------------------------------

data ARInstr c a where
    NewAddress              :: ARInstr c Address
    NewProcess              :: Proc -> ARInstr c ()
    NewProbe                :: String -> (Label -> Maybe Value) -> ARInstr c ()
    NewInit                 :: Address -> Value -> ARInstr c ()
    Component               :: (forall c. AR c a) -> ARInstr c a
    Connect                 :: Connectable a b => a -> b -> ARInstr c ()

type AR c a                 = Program (ARInstr c) a

runAR                       :: (forall c . AR c a) -> SimState -> (a,SimState)
runAR sys st                = run sys st
  where
    run                     :: AR c a -> SimState -> (a,SimState)
    run sys st              = run' (view sys) st
    run'                    :: ProgramView (ARInstr c) a -> SimState -> (a,SimState)
    run' (NewAddress :>>= sys) st
                            = run (sys (nextA st)) (st { nextA = nextA st + 1 })
    run' (NewProcess p :>>= sys) st
                            = run (sys ()) (st { procs = p : procs st })
    run' (NewProbe s f :>>= sys) st
                            = run (sys ()) (st { probes = (s,f) : probes st })
    run' (NewInit a v :>>= sys) st
                            = run (sys ()) (st { initvals = Map.insert a v (initvals st) })
    run' (Component subsys :>>= sys) st
                            = let (a,st') = runAR subsys st in run (sys a) st'
    run' (Connect a b :>>= sys) st
                            = run (sys ()) (st { conns = connection a b : conns st })
    run' (Return a) st      = (a,st)

initialize                  :: (forall c . AR c a) -> (a,SimState)
initialize sys              = (a, st { procs = map (apInit (conns st) (initvals st)) (procs st) })
  where (a,st)              = runAR sys state0

-- Restricting connections ----------------------------------------------------

class Connectable a b | a -> b, b -> a where
    connection              :: a -> b -> Conn

instance Connectable (ProvidedDataElem a) (RequiredDataElem a) where
    connection (PE a) (RE b)    = (a,b)
        
instance Connectable (ProvidedQueueElem a) (RequiredQueueElem a) where
    connection (PQ a) (RQ b)    = (a,b)
        
instance Connectable (RequiredOp a b) (ProvidedOp a b) where
    connection (RO a) (PO b)    = (a,b)


class Addressed a where
        address                     :: [a] -> [Address]

instance Addressed (ProvidedDataElement a c) where
        address xs                  = [ n | PE n <- xs ]

instance Addressed (RequiredDataElement a c) where
        address xs                  = [ n | RE n <- xs ]
        
instance Addressed (ProvidedQueueElement a c) where
        address xs                  = [ n | PQ n <- xs ]

instance Addressed (RequiredQueueElement a c) where
        address xs                  = [ n | RQ n <- xs ]
        
instance Addressed (ProvidedOperation a b c) where
        address xs                  = [ n | PO n <- xs ]

instance Addressed (RequiredOperation a b c) where
        address xs                  = [ n | RO n <- xs ]



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

requiredDataElement         :: AR c (RequiredDataElement a c)
requiredDataElementInit     :: Data a => a -> AR c (RequiredDataElement a c)
providedDataElement         :: AR c (ProvidedDataElement a c)
providedDataElementInit     :: Data a => a -> AR c (ProvidedDataElement a c)
requiredQueueElement        :: Int -> AR c (RequiredQueueElement a c)
providedQueueElement        :: AR c (ProvidedQueueElement a c)
requiredOperation           :: AR c (RequiredOperation a b c)
providedOperation           :: AR c (ProvidedOperation a b c)
interRunnableVariable       :: Data a => a -> AR c (InterRunnableVariable a c)
exclusiveArea               :: AR c (ExclusiveArea c)
runnable                    :: Invocation -> [Trigger c] -> RTE c a -> AR c ()
serverRunnable              :: (Data a, Data b) => 
                                Invocation -> [ProvidedOperation a b c] -> (a -> RTE c b) -> AR c ()
component                   :: (forall c. AR c a) -> AR c a
connect                     :: Connectable a b => a -> b -> AR c ()

component c                 = singleton $ Component c
connect a b                 = singleton $ Connect a b

newAddress                  = singleton NewAddress
newProcess p                = singleton $ NewProcess p
newInit a v                 = singleton $ NewInit a v

requiredDataElement         = do a <- newAddress; newProcess (DElem a False NO_DATA); return (RE a)
requiredDataElementInit val = do a <- newAddress; newProcess (DElem a False (Ok (toValue val))); return (RE a)
providedDataElement         = do a <- newAddress; return (PE a)
providedDataElementInit val = do a <- newAddress; newInit a (toValue val); return (PE a)
requiredQueueElement size   = do a <- newAddress; newProcess (QElem a size []); return (RQ a)
providedQueueElement        = do a <- newAddress; return (PQ a)
requiredOperation           = do a <- newAddress; newProcess (Op a []); return (RO a)
providedOperation           = do a <- newAddress; return (PO a)
interRunnableVariable val   = do a <- newAddress; newProcess (Irv a (toValue val)); return (IV a)
exclusiveArea               = do a <- newAddress; newProcess (Excl a Free); return (EX a)

runnable inv trig code      = do a <- newAddress
                                 mapM (newProcess . Timer a 0.0) periods
                                 newProcess (Run a 0.0 act 0 (Static watch inv code'))
  where periods             = [ t | Timed t <- trig ]
        watch               = [ a | ReceiveE (RE a) <- trig ] ++ [ a | ReceiveQ (RQ a) <- trig ]
        act                 = if null [ Init | Init <- trig ] then Idle else Pending
        code'               = \dyn -> code >> return dyn

serverRunnable inv ops code = do a <- newAddress
                                 newProcess (Run a 0.0 act 0 (Static watch inv code'))
  where watch               = [ a | PO a <- ops ]
        act                 = Serving [] []
        code'               = fmap toValue . code . fromDyn

fromDyn                     :: Data a => Value -> a
fromDyn                     = value'

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


-- probeLabels :: Addressed a => 

probeRead                   :: Data a => String -> RequiredDataElement a c -> AR c' ()
probeRead s (RE a)        = singleton $ NewProbe s g
  where 
    g (RD b (Ok v)) | a==b  = Just v
    g _                     = Nothing

probeWrite                  :: Data a => String -> ProvidedDataElement a c -> AR c' ()
probeWrite s p            = probeWrite' s p id

probeWrite'                  :: (Data b, Data a) => String -> ProvidedDataElement a c -> (a -> b) -> AR c' ()
probeWrite' s (PE a) f    = singleton $ NewProbe s g
  where 
    g (WR b v) | a==b       = Just (toValue $ f $ value' v)
    g _                     = Nothing

data Label                  = ENTER Address
                            | EXIT  Address
                            | IRVR  Address (StdRet Value)
                            | IRVW  Address Value
                            | RCV   Address (StdRet Value)
                            | SND   Address Value (StdRet Value)
                            | RD    Address (StdRet Value)
                            | WR    Address Value
                            | UP    Address (StdRet Value)
                            | INV   Address
                            | CALL  Address Value (StdRet Value)
                            | RES   Address (StdRet Value)
                            | RET   Address Value
                            | NEW   Address
                            | TERM  Address
                            | TICK  Address
                            | DELTA Time
                            | VETO

labelText :: Label -> String
labelText l = case l of
          ENTER a            -> "ENTER:"++show a
          EXIT  a            -> "EXIT:"++show a
          IRVR  a ret        -> "IRVR:"++show a
          IRVW  a val        -> "IRVW:"++show a++":"++show val
          RCV   a ret        -> "RCV:"++show a
          SND   a val ret    -> "SND:"++show a++":"++show val
          RD    a ret        -> "RD:"++show a
          WR    a val        -> "WR:"++show a++":"++show val
          UP    a ret        -> "UP:"++show a
          INV   a            -> "INV:"++show a
          CALL  a val ret    -> "CALL:"++show a++":"++show val
          RES   a ret        -> "RES:"++show a
          RET   a val        -> "RET:"++show a++":"++show val
          NEW   a            -> "NEW:"++show a
          TERM  a            -> "TERM:"++show a
          TICK  a            -> "TICK:"++show a
          DELTA t            -> "DELTA:"++show t
          VETO               -> "VETO"

maySay :: Proc -> Label
maySay (Run a 0.0 Pending n s)
    | n == 0 || invocation s == Concurrent     = NEW   a
maySay (Run a 0.0 (Serving (c:cs) (v:vs)) n s)    
    | n == 0 || invocation s == Concurrent     = NEW   a
maySay (Run a t act n s)  | t > 0.0            = DELTA t
maySay (Timer a 0.0 t)                         = TICK  a
maySay (Timer a t t0)     | t > 0.0            = DELTA t
maySay (RInst a c ex code)                     = maySay' (view code)
  where maySay' (Enter (EX x)      :>>= cont)  = ENTER x
        maySay' (Exit  (EX x)      :>>= cont)  = case ex of
                                                     y:ys | y==x -> EXIT x
                                                     _           -> VETO
        maySay' (IrvRead  (IV s)   :>>= cont)  = IRVR  s NO_DATA
        maySay' (IrvWrite (IV s) v :>>= cont)  = IRVW  s (toValue v)
        maySay' (Receive (RQ e)    :>>= cont)  = RCV   e NO_DATA
        maySay' (Send    (PQ e) v  :>>= cont)  = SND   e (toValue v) ok
        maySay' (Read    (RE e)    :>>= cont)  = RD    e NO_DATA
        maySay' (Write   (PE e) v  :>>= cont)  = WR    e (toValue v)
        maySay' (IsUpdated  (RE e) :>>= cont)  = UP    e NO_DATA
        maySay' (Invalidate (PE e) :>>= cont)  = INV   e
        maySay' (Call   (RO o) v   :>>= cont)  = CALL  o (toValue v) NO_DATA
        maySay' (Result (RO o)     :>>= cont)  = RES   o NO_DATA
        maySay' (Return v)                     = case c of
                                                     Just b  -> RET  b v
                                                     Nothing -> TERM a
        maySay' (Printlog s        :>>= cont)  = maySay' (view (cont ()))
maySay _                                       = VETO   -- most processes can't say anything


say :: Label -> Proc -> [Proc]
say (NEW _)   (Run a _ Pending n s)                     = [Run a (minstart s) Idle (n+1) s,
                                                           RInst a Nothing [] (implementation s (toValue ()))]
say (NEW _)   (Run a _ (Serving (c:cs) (v:vs)) n s)     = [Run a (minstart s) (Serving cs vs) (n+1) s,
                                                           RInst a (Just c) [] (implementation s v)]
say (DELTA d) (Run a t act n s)                         = [Run a (t-d) act n s]
say (TICK _)  (Timer a _ t)                             = [Timer a t t]
say (DELTA d) (Timer a t t0)                            = [Timer a (t-d) t0]
say label     (RInst a c ex code)                       = say' label (view code)
  where say' (ENTER _)      (Enter (EX x) :>>= cont)    = [RInst a c (x:ex)   (cont void)]
        say' (EXIT _)       (Exit (EX x)  :>>= cont)    = [RInst a c ex       (cont void)]
        say' (IRVR _ res)   (IrvRead _    :>>= cont)    = [RInst a c ex       (cont (fromStdDyn res))]
        say' (IRVW _ _)     (IrvWrite _ _ :>>= cont)    = [RInst a c ex       (cont void)]
        say' (RCV _ res)    (Receive _    :>>= cont)    = [RInst a c ex       (cont (fromStdDyn res))]
        say' (SND _ _ res)  (Send _ _     :>>= cont)    = [RInst a c ex       (cont (fromStdDyn res))]
        say' (RD _ res)     (Read _       :>>= cont)    = [RInst a c ex       (cont (fromStdDyn res))]
        say' (WR _ _)       (Write _ _    :>>= cont)    = [RInst a c ex       (cont void)]
        say' (UP _ res)     (IsUpdated _  :>>= cont)    = [RInst a c ex       (cont (fromStdDyn res))]
        say' (INV _)        (Invalidate _ :>>= cont)    = [RInst a c ex       (cont void)]
        say' (CALL _ _ res) (Call _ _     :>>= cont)    = [RInst a c ex       (cont (fromStdDyn res))]
        say' (RES _    res) (Result _     :>>= cont)    = [RInst a c ex       (cont (fromStdDyn res))]
        say' (RET _ _)      (Return v)                  = [RInst a Nothing ex (return (toValue ()))]
        say' (TERM _)       (Return _)                  = []
        say' label          (Printlog s   :>>= cont)    = say' label (view (cont ()))


mayLog (RInst a c ex code)                              = mayLog' (view code)
  where mayLog' :: ProgramView (RTEop c) a -> [String]
        mayLog' (Printlog s :>>= cont)                  = s : mayLog' (view (cont ()))
        mayLog' _                                       = []
mayLog _                                                = []


ok   :: StdRet Value
ok              = Ok (toValue ())

void :: StdRet ()
void            = Ok ()

fromStdDyn :: Data a => StdRet Value -> StdRet a
fromStdDyn (Ok v)   = Ok (fromDyn v)
fromStdDyn NO_DATA  = NO_DATA
fromStdDyn LIMIT    = LIMIT

minstart :: Static c -> Time
minstart s      = case invocation s of
                    MinInterval t -> t
                    Concurrent    -> 0.0

trig :: ConnRel -> Address -> Static c -> Bool
trig conn a s   = or [ a `conn` b | b <- triggers s ]


mayHear :: ConnRel -> Label -> Proc -> Label
mayHear conn VETO _                                             = VETO
mayHear conn (ENTER a)       (Excl b Free)     | a==b           = ENTER a -- 
mayHear conn (ENTER a)       (Excl b _)        | a==b           = VETO
mayHear conn (EXIT a)        (Excl b Taken)    | a==b           = EXIT a
mayHear conn (EXIT a)        (Excl b _)        | a==b           = VETO
mayHear conn (IRVR a _)      (Irv b v)         | a==b           = IRVR a (Ok v)
mayHear conn (IRVW a v)      (Irv b _)         | a==b           = IRVW a v
mayHear conn (RCV a _)       (QElem b n (v:_)) | a==b           = RCV a (Ok v)
mayHear conn (RCV a _)       (QElem b n [])    | a==b           = RCV a NO_DATA
mayHear conn (SND a v res)   (QElem b n vs)                     
       | a `conn` b && length vs < n                            = SND a v res
       | a `conn` b                                             = SND a v LIMIT
mayHear conn (SND a v res)   (Run _ _ _ _ s)   | trig conn a s  = SND a v res
mayHear conn (RD a _)        (DElem b u v)     | a==b           = RD a v
mayHear conn (WR a v)        (DElem b _ _)     | a `conn` b     = WR a v
mayHear conn (WR a v)        (Run _ _ _ _ s)   | trig conn a s  = WR a v
mayHear conn (UP a _)        (DElem b u _)     | a==b           = UP a (Ok (toValue u))
mayHear conn (INV a)         (DElem b _ _)     | a `conn` b     = INV a
mayHear conn (CALL a v res)  (Run b t (Serving cs vs) n s)      
       | trig conn a s  &&  a `notElem` cs                      = CALL a v ok
       | trig conn a s                                          = CALL a v LIMIT
mayHear conn (RES a _)       (Op b (v:vs))     | a==b           = RES a (Ok v)
--mayHear conn (RES a _)       (Op b [])         | a==b           = RES a NO_DATA
mayHear conn (RES a _)       (Op b [])         | a==b           = VETO
mayHear conn (RET a v)       (Op b vs)         | a==b           = RET a v
mayHear conn (TERM a)        (Run b _ _ _ _)   | a==b           = TERM a
mayHear conn (TICK a)        (Run b _ _ _ _)   | a==b           = TICK a
mayHear conn (DELTA d)       (Run _ t _ _ _)   | t == 0         = DELTA d
                                               | d <= t         = DELTA d
                                               | d > t          = VETO
mayHear conn (DELTA d)       (Timer _ t _)     | d <= t         = DELTA d
                                               | d > t          = VETO
mayHear conn label _                                            = label


hear :: ConnRel -> Label -> Proc -> Proc
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


step :: ConnRel -> [Proc] -> [SchedulerOption]
step conn procs        = explore conn [] labels procs
  where labels         = map  (respond . maySay)     procs
        respond label  = foldl (mayHear conn) label  procs

explore :: ConnRel -> [Proc] -> [Label] -> [Proc] -> [SchedulerOption]
explore conn pre (VETO:labels) (p:post) = explore conn (p:pre) labels post
explore conn pre (l:labels)    (p:post) = commit : explore conn (p:pre) labels post
  where commit                          = (l, logs l, map (hear conn l) pre ++ say l p ++ map (hear conn l) post)
        logs (DELTA _)                  = []
        logs _                          = mayLog p
explore conn _ _ _                      = []


-- The simulator proper ---------------------------------------------------------------------------------------

type Logs                   = [String]

type SchedulerOption        = (Label, Logs, [Proc])
data Transition             = Trans {transChoice  :: Int
                                    , transLabel  :: Label
                                    , transLogs   :: Logs
                                    , transProcs  :: [Proc]}
type Scheduler m            = [SchedulerOption] -> m Transition
type Trace                  = (SimState, [Transition])

simulation                  :: Monad m => Scheduler m -> (forall c . AR c a) -> m (a,Trace)
simulation sched sys        = do trs <- simulate sched conn (procs state1)
                                 return (res, (state1,trs))
  where (res,state1)        = initialize sys
        a `conn` b          = (a,b) `elem` conns state1

simulate sched conn procs
  | null alts               = return []
  | otherwise               = do trans@Trans{transProcs = procs1} <- maximumProgress sched alts
                                 liftM (trans:) $ simulate sched conn procs1
  where alts                = step conn procs
        
maximumProgress             :: Scheduler m -> Scheduler m
maximumProgress sched alts
  | null work               = sched deltas
  | otherwise               = sched work
  where (deltas,work)       = partition isDelta alts
        isDelta (DELTA _,_,_) = True
        isDelta _             = False

trivialSched                :: Scheduler Identity
trivialSched alts           = return (Trans 0 label logs procs)
  where (label,logs,procs)  = head alts

roundRobinSched             :: Scheduler (State Int)
roundRobinSched alts        = do m <- get
                                 let n = (m+1) `mod` length alts
                                     (label,logs,procs) = alts!!n
                                 put n
                                 return (Trans n label logs procs)

randomSched                 :: Scheduler (State StdGen)
randomSched alts            = do n <- state next
                                 let (label,logs,procs) = alts!!(n `mod` length alts)
                                 return (Trans n label logs procs)


data SchedChoice            where
  TrivialSched        :: SchedChoice
  RoundRobinSched     :: SchedChoice
  RandomSched         :: StdGen -> SchedChoice
  -- This can be used to define the all the other cases
  AnySched            :: Monad m => Scheduler m -> (forall a. m a -> a) -> SchedChoice


runSim                         :: SchedChoice -> (forall c . AR c a) -> (a,Trace)
runSim TrivialSched sys        = runIdentity (simulation trivialSched sys)
runSim RoundRobinSched sys     = evalState (simulation roundRobinSched sys) 0
runSim (RandomSched g) sys     = evalState (simulation randomSched sys) g
runSim (AnySched sch run) sys  = run (simulation sch sys)

{-
<<<<<<< Updated upstream
data Output                 = Output {
                                  measurements :: [(String,Measurement)],
                                  logs         :: [(Time,String)]
                              }

runARSim                    :: SchedChoice -> Double -> (forall c . AR c a) -> Output
runARSim choice limit sys   = Output {
                                  measurements = rearrange $ collectMeas limit (probes state) 0.0 trs,
                                  logs = collectLogs limit 0.0 trs
                              }
  where (_,(state,trs))     = runSim choice sys

rearrange                   = Map.toList . Map.fromListWith (flip (++))

collectMeas lim probes t [] = []
collectMeas lim probes t _
  | t > lim                 = []
collectMeas lim probes t ((_,DELTA d,_,_):trs)
                            = collectMeas lim probes (t+d) trs
collectMeas lim probes t ((_,label,_,_):trs)
                            = measurements ++ collectMeas lim probes t trs
  where measurements        = [ (s,[(t,v)]) | (s,f) <- probes, Just v <- [f label] ]

collectLogs lim t []        = []
collectLogs lim t _
  | t > lim                 = []
collectLogs lim t ((_,DELTA d,_,_):trs)
                            = collectLogs lim (t+d) trs
collectLogs lim t ((_,_,ls,_):trs)
                            = [ (t,v) | v <- ls ] ++ collectLogs lim t trs
=======
-}
limitTicks :: Int -> Trace -> Trace 
limitTicks t (a,trs) = (a,take t trs)

limitTime :: Time -> Trace -> Trace
limitTime t (a,trs) = (a,limitTimeTrs t trs) where
  limitTimeTrs t _ | t < 0                 = []
  limitTimeTrs t (del@(Trans{transLabel = DELTA d}):trs) = del:limitTimeTrs (t-d) trs
  limitTimeTrs t []                        = []
  limitTimeTrs t (x:xs)                    = x : limitTimeTrs t xs


type Measurement a           = [((Int,Time),a)] -- The int is the number of transitions

-- Gets ALL probes of a certain types, categorized by probe-ID. 
-- This function is strict in the trace, so limitTicks and/or limitTime should be used for infinite traces. 
runARSim :: Data a => Trace -> [(String,Measurement a)]
runARSim t = [(s,internal m) |(s,m) <- runARSim' t] 

internal :: Data a => Measurement Value -> Measurement a
internal ms = [(t,a)|(t,v) <- ms, Just a <- return (value v)]

runARSim'                    :: Trace -> [(String,Measurement Value)]
runARSim' (state,trs)   = Map.toList $ Map.fromListWith (++) $ collect (probes state) 0.0 0 trs


collect :: [Probe] -> Time -> Int -> [Transition] -> [(String,Measurement Value)]
collect probes t n []     = []
collect probes t n (Trans{transLabel = DELTA d}:trs)
                            = collect probes (t+d) (n+1) trs
collect probes t n (Trans{transLabel = label}:trs)
                            = measurements ++ collect probes t (n+1) trs
  where measurements        = [ (s,[((n,t),v)]) | (s,f) <- probes, Just v <- [f label] ]

-- Run a simulation with a time limit, returning all probes of a type a. 
timedARSim :: Data a => SchedChoice -> Time -> (forall c . AR c x) -> [(String,Measurement a)]
timedARSim sch t sys = runARSim (snd $ runSim sch sys)
