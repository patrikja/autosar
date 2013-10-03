{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies,
             Rank2Types, ExistentialQuantification #-}

module ARSim (RunM, RE, PE, RQ, PQ, RO, PO, IV, EX, Value, Valuable, StdRet(..),
              rte_send, rte_receive, rte_write, rte_read, rte_isUpdated, rte_invalidate, 
              rte_call, rte_callAsync, rte_result, rte_irvWrite, rte_irvRead, rte_enter, rte_exit,
              AR, Time, Trigger(..), Invocation(..), component, runnable, serverRunnable,
              requiredDataElement, providedDataElement, requiredQueuedElement, providedQueuedElement,
              requiredOperation, providedOperation, interRunnableVariable, exclusiveArea,
              Seal(..), seal2, seal3, seal4, seal5, seal6, seal7,
              Connect(..), connect2, connect3, connect4, connect5, connect6, connect7,
              simulation) where


newtype RunM a          = RunM (Con a -> Code)

instance Monad RunM where
        RunM f >>= b    = RunM (\cont -> f (\x -> let RunM g = b x in g cont))
        return r        = RunM (\cont -> cont r)

-- Phatom type arguments to make the use more type safe
newtype RE a c          = RE (InstName, ElemName)
newtype PE a c          = PE (InstName, ElemName)
newtype RQ a c          = RQ (InstName, ElemName)
newtype PQ a c          = PQ (InstName, ElemName)
newtype RO a b c        = RO (InstName, OpName)
newtype PO a b c        = PO (InstName, OpName)
newtype IV a c          = IV (InstName, VarName)
newtype EX c            = EX (InstName, ExclName)

data Value      = Void
                | VBool     Bool
                | VInt      Int
                | VReal     Double
                | VString   String
                | VArray    [Value]
                deriving (Eq,Ord,Show)

class Valuable a where
        toVal                   :: a -> Value
        fromVal                 :: Value -> a

instance Valuable () where
        toVal ()                = Void
        fromVal Void            = ()

instance Valuable Bool where
        toVal                   = VBool
        fromVal (VBool v)       = v

instance Valuable Int where
        toVal                   = VInt
        fromVal (VInt v)        = v

instance Valuable Double where
        toVal                   = VReal
        fromVal (VReal v)       = v

instance Valuable [Char] where
        toVal                   = VString
        fromVal (VString v)     = v

instance Valuable a => Valuable [a] where
        toVal                   = VArray . map toVal
        fromVal (VArray vs)     = map fromVal vs

data StdRet a   = Ok a
                | NO_DATA
                | NEVER_RECEIVED
                | LIMIT
                | UNCONNECTED
                | TIMEOUT
                | IN_EXCLUSIVE_AREA
                deriving (Eq,Ord,Show)

instance Functor StdRet where
        fmap f (Ok v)            = Ok (f v)
        fmap f NO_DATA           = NO_DATA
        fmap f NEVER_RECEIVED    = NEVER_RECEIVED
        fmap f LIMIT             = LIMIT
        fmap f UNCONNECTED       = UNCONNECTED
        fmap f TIMEOUT           = TIMEOUT
        fmap f IN_EXCLUSIVE_AREA = IN_EXCLUSIVE_AREA

rte_send        :: Valuable a => PE a c -> a -> RunM (StdRet ())
rte_receive     :: Valuable a => RE a c -> RunM (StdRet a)
rte_write       :: Valuable a => PE a c -> a -> RunM (StdRet ())
rte_read        :: Valuable a => RE a c -> RunM (StdRet a)
rte_isUpdated   :: Valuable a => RE a c -> RunM (StdRet Bool)
rte_invalidate  :: Valuable a => PE a c -> RunM (StdRet ())
rte_call        :: (Valuable a, Valuable b) => RO a b c -> a -> RunM (StdRet b)
rte_callAsync   :: (Valuable a, Valuable b) => RO a b c -> a -> RunM (StdRet b)
rte_result      :: (Valuable a, Valuable b) => RO a b c -> RunM (StdRet b)
rte_irvWrite    :: Valuable a => IV a c -> a -> RunM (StdRet ())
rte_irvRead     :: Valuable a => IV a c -> RunM (StdRet a)
rte_enter       :: EX c -> RunM (StdRet ())
rte_exit        :: EX c -> RunM (StdRet ())


-- A monad for building up the static structure. Accumulates the
-- ProcessSoup building up.
newtype AR c a          = AR (State -> (a,State))

instance Monad (AR c) where
        AR f >>= b      = AR (\s -> let (x,t) = f s; AR g = b x in g t)
        return x        = AR (\s -> (x,s))

type Time               = Double

data Trigger c          = forall a. ReceiveE (RE a c)
                        | forall a. ReceiveQ (RQ a c)
                        | Timed Time

data Invocation         = Concurrent
                        | MinInterval Time
                        deriving (Eq,Show)


requiredDataElement     :: AR c (RE a c)
providedDataElement     :: AR c (PE a c)
requiredQueuedElement   :: Int -> AR c (RQ a c)
providedQueuedElement   :: AR c (PQ a c)
requiredOperation       :: AR c (RO a b c)
providedOperation       :: AR c (PO a b c)
interRunnableVariable   :: Valuable a => a -> AR c (IV a c)
exclusiveArea           :: AR c (EX c)
runnable                :: Invocation -> [Trigger c] -> RunM (StdRet ()) -> AR c ()
serverRunnable          :: (Valuable a, Valuable b) => 
                           Invocation -> [PO a b c] -> (a -> RunM (StdRet b)) -> AR c ()

component               :: (forall c. AR c a) -> AR c a


-- A hack to do some kind of subtype coercion. 
class Seal m where
        seal            :: m c -> m ()

instance Seal (RE a) where
        seal (RE a)    = RE a

instance Seal (PE a) where
        seal (PE a)    = PE a

instance Seal (RQ a) where
        seal (RQ a)    = RQ a

instance Seal (PQ a) where
        seal (PQ a)    = PQ a

instance Seal (RO a b) where
        seal (RO a)    = RO a

instance Seal (PO a b) where
        seal (PO a)    = PO a

seal2 (a1,a2)                   = (seal a1, seal a2)
seal3 (a1,a2,a3)                = (seal a1, seal a2, seal a3)
seal4 (a1,a2,a3,a4)             = (seal a1, seal a2, seal a3, seal a4)
seal5 (a1,a2,a3,a4,a5)          = (seal a1, seal a2, seal a3, seal a4, seal a5)
seal6 (a1,a2,a3,a4,a5,a6)       = (seal a1, seal a2, seal a3, seal a4, seal a5, seal a6)
seal7 (a1,a2,a3,a4,a5,a6,a7)    = (seal a1, seal a2, seal a3, seal a4, seal a5, seal a6, seal a7)

class Connect a b | a -> b where
        connect                 :: a -> b -> AR c ()

instance Connect (PE a ()) (RE a ()) where
        connect (PE a) (RE b)   = addConn (a,b)
        
instance Connect (PQ a ()) (RQ a ()) where
        connect (PQ a) (RQ b)   = addConn (a,b)
        
instance Connect (PO a b ()) (RO a b ()) where
        connect (PO a) (RO b)   = addConn (a,b)
        
connect2 (a1,a2) (b1,b2)        
                                = do connect a1 b1; connect a2 b2
connect3 (a1,a2,a3) (b1,b2,b3)  
                                = do connect a1 b1; connect (a2,a3) (b2,b3)
connect4 (a1,a2,a3,a4) (b1,b2,b3,b4)  
                                = do connect a1 b1; connect (a2,a3,a4) (b2,b3,b4)
connect5 (a1,a2,a3,a4,a5) (b1,b2,b3,b4,b5)
                                = do connect a1 b1; connect (a2,a3,a4,a5) (b2,b3,b4,b5)
connect6 (a1,a2,a3,a4,a5,a6) (b1,b2,b3,b4,b5,b6)  
                                = do connect a1 b1; connect (a2,a3,a4,a5,a6) (b2,b3,b4,b5,b6)
connect7 (a1,a2,a3,a4,a5,a6,a7) (b1,b2,b3,b4,b5,b6,b7)
                                = do connect a1 b1; connect (a2,a3,a4,a5,a6,a7) (b2,b3,b4,b5,b6,b7)


simulation :: Int -> (forall c. AR c a) -> String


-----------------------------------------------------------------------------------------------------

type Name       = Int
type InstName   = Name
type OpName     = Name
type ElemName   = Name
type VarName    = Name
type ExclName   = Name
type RunName    = Name

void            :: StdRet Value
void            = Ok Void

type StdReturn  = StdRet Value

type Cont       = StdReturn -> Code

data Code       = Send ElemName Value Cont
                | Receive ElemName Cont
                | Write ElemName Value Cont
                | Read ElemName Cont
                | IsUpdated ElemName Cont
                | Invalidate ElemName Cont
                | Call OpName Value Cont
                | Result OpName Cont
                | IrvWrite VarName Value Cont
                | IrvRead VarName Cont
                | Enter ExclName Cont
                | Exit ExclName Cont
                | Terminate StdReturn
                
type Con a              = a -> Code

runM                    :: (Valuable a, Valuable b) => (a -> RunM (StdRet b)) -> Value -> Code
runM m v                = let RunM f = m (fromVal v) in f (\r -> Terminate (toStd r))


rte_send (PE (_,e)) v       = RunM (\cont -> Send e (toVal v) (cont . fromStd))

rte_receive (RE (_,e))      = RunM (\cont -> Receive e (cont . fromStd))

rte_write (PE (_,e)) v      = RunM (\cont -> Write e (toVal v) (cont . fromStd))

rte_read (RE (_,e))         = RunM (\cont -> Read e (cont . fromStd))

rte_isUpdated (RE (_,e))    = RunM (\cont -> IsUpdated e (cont . fromStd))

rte_invalidate (PE (_,e))   = RunM (\cont -> Invalidate e (cont . fromStd))

rte_call (RO (_,o)) v       = RunM (\cont -> Call o (toVal v) (f cont))
  where f cont r            = case r of Ok Void -> Result o (cont . fromStd); _ -> cont (fromStd r)

rte_callAsync (RO (_,o)) v  = RunM (\cont -> Call o (toVal v) (cont . fromStd))

rte_result (RO (_,o))       = RunM (\cont -> Result o (cont . fromStd))

rte_irvWrite (IV (_,s)) v   = RunM (\cont -> IrvWrite s (toVal v) (cont . fromStd))

rte_irvRead (IV (_,s))      = RunM (\cont -> IrvRead s (cont . fromStd))

rte_enter (EX (_,x))        = RunM (\cont -> Enter x (cont . fromStd))

rte_exit (EX (_,x))         = RunM (\cont -> Exit x (cont . fromStd))


toStd :: Valuable a => StdRet a -> StdReturn
toStd = fmap toVal

fromStd :: Valuable a => StdReturn -> StdRet a
fromStd = fmap fromVal

------------------------------------

type Name2              = (InstName,Name)

type Conn               = (Name2,Name2)

-- TODO: distinguish or name types for the Ints?
-- TODO: deriving Show etc. fails due to Cont being a function type
data State              = State {
                                inst    :: Int,
                                next    :: Int,
                                procs   :: [Proc],
                                conns   :: [Conn]
                          }

startState              :: State
startState              = State 0 0 [] []

runAR                   :: (forall c. AR c a) -> State -> (a, [Proc], [Conn])
runAR (AR m) s          = let (r,t) = m s in (r, procs t, conns t)

get                     :: AR c State
get                     = AR (\s -> (s,s))

put                     :: State -> AR c ()
put s                   = AR (\_ -> ((),s))

newName                 :: AR c (InstName, Name)
newName                 = do s <- get
                             put (s {next = next s + 1})
                             return (inst s,next s)

addProc                 :: Proc -> AR c ()
addProc p               = do s <- get
                             put (s {procs = p:procs s})

addConn                 :: Conn -> AR c ()
addConn c               = do s <- get
                             put (s {conns = c   : conns s})


component m             = do s <- get
                             let (r,ps,cs) = runAR m (s {inst = next s, next = 0})
                             put (s {next = next s + 1, procs = ps, conns = cs})
                             return r

runnable inv tr m       = do a <- newName
                             mapM (addProc . Timer a 0.0) ts
                             addProc (Run a 0.0 Idle 0 (static inv ns (runM mfun)))
        where ns        = [ n | ReceiveE (RE n) <- tr ] ++ [ n | ReceiveQ (RQ n) <- tr ]
              ts        = [ t | Timed t <- tr ]
              mfun      :: () -> RunM (StdRet ())
              mfun _    = m

serverRunnable inv tr m = do a <- newName
                             addProc (Run a 0.0 (Serving [] []) 0 (static inv ns (runM m)))
        where ns        = [ n | PO n <- tr ]

requiredDataElement     = do a <- newName
                             addProc (DElem a False NO_DATA)
                             return (RE a)

providedDataElement     = do a <- newName
                             return (PE a)

requiredQueuedElement n = do a <- newName
                             addProc (QElem a n [])
                             return (RQ a)

providedQueuedElement   = do a <- newName
                             return (PQ a)

requiredOperation       = do a <- newName
                             addProc (Op a [])
                             return (RO a)

providedOperation       = do a <- newName
                             return (PO a)

interRunnableVariable v = do a <- newName
                             addProc (Irv a (toVal v))
                             return (IV a)

exclusiveArea           = do a <- newName
                             addProc (Excl a True)
                             return (EX a)

--------------------------------------------------------------------------------

r1 ppe                  = do rte_write ppe (123::Int)

c1                      = do ppe <- providedDataElement
                             rpe <- requiredDataElement
                             runnable Concurrent [ReceiveE rpe] (r1 ppe)
                             return (seal ppe)

r2 False                = return (Ok True)
r2 True                 = return (Ok False)

c2                      = do p1 <- providedDataElement
                             rpo1 <- providedOperation
                             rpo2 <- providedOperation
                             serverRunnable (MinInterval 0.3) [rpo1,rpo2] r2
                             p2 <- requiredDataElement
                             return (seal p1, seal p2)

tsst                    = do ppe <- component c1
                             (p1,p2) <- component c2
                             connect ppe p2
                             
--------------------------------------------------------------------------------

type Client     = (InstName, OpName)

data Act        = Idle
                | Pending
                | Serving [Client] [Value]
                deriving (Eq, Ord, Show)

data Static     = Static {
                        triggers        :: [Name2],
                        invocation      :: Invocation,
                        implementation  :: Value -> Code
                  }        

static          :: Invocation -> [Name2] -> (Value -> Code) -> Static
static inv ns f = Static { triggers = ns, invocation = inv, implementation = f }

minstart        :: Static -> Time
minstart s      = case invocation s of
                        MinInterval t -> t
                        Concurrent    -> 0.0

type Connected  = Name2 -> Name2 -> Bool

connected       :: [Conn] -> Connected
connected cs    = \a b -> (a,b) `elem` cs

trig :: Connected -> Name2 -> Static -> Bool
trig conn a s   = or [ a `conn` b | b <- triggers s ]

-- PJ: Why pairs when there are newtypes RE, IV, etc. defined above? 
-- JN: This is the untyped "machine" representation. RE, IV, et al. just add safety
-- by means of phantom types. Types would be very inconvenient in a list of Proc terms, 
-- though, since every Proc element might require a distinct type. And I don't think
-- we want to enclose each Proc behind an existential quantifier.
data Proc       = Run   (InstName, RunName) Time Act Int Static
                | RInst (InstName, RunName) (Maybe Client) [ExclName] Code
                | Excl  (InstName, ExclName) Bool
                | Irv   (InstName, VarName) Value
                | Timer (InstName, RunName) Time Time
                | QElem (InstName, ElemName) Int [Value]
                | DElem (InstName, ElemName) Bool StdReturn
                | Op    (InstName, OpName) [Value]

data Label      = ENTER (InstName, ExclName)
                | EXIT  (InstName, ExclName)
                | IRVR  (InstName, VarName) StdReturn
                | IRVW  (InstName, VarName) Value
                | RCV   (InstName, ElemName) StdReturn
                | SND   (InstName, ElemName) Value StdReturn
                | RD    (InstName, ElemName) StdReturn
                | WR    (InstName, ElemName) Value
                | UP    (InstName, ElemName) StdReturn
                | INV   (InstName, ElemName)
                | CALL  (InstName, OpName) Value StdReturn
                | RES   (InstName, OpName) StdReturn
                | RET   (InstName, OpName) Value
                | NEW   (InstName, RunName)
                | TERM  (InstName, RunName)
                | TICK  (InstName, RunName)
                | DELTA Time
                | PASS
                deriving (Eq, Ord, Show)

simulation n m          = let (_,ps,cs) = runAR m startState 
                          in simulate (scheduler n) (connected cs) ps

type Scheduler          = [(Label,[Proc])] -> (Label,[Proc])

scheduler :: Int -> Scheduler
scheduler 0             = head

simulate :: Scheduler -> Connected -> [Proc] -> String
simulate sched conn procs     
  | null next           = "\n"
  | otherwise           = show l ++ "\n" ++ simulate sched conn procs'
  where next            = step conn procs
        (l,procs')      = sched next

-- This provides all possible results, a "scheduler" then needs to pick one.
step :: Connected -> [Proc] -> [(Label, [Proc])]
step conn procs                         = explore conn [] labels1 procs
  where labels0                         = map may_say procs
        labels1                         = map (respond procs) labels0
        respond procs label             = foldl (may_hear conn) label procs

explore :: Connected -> [Proc] -> [Label] -> [Proc] -> [(Label, [Proc])]
explore conn pre (PASS:labels) (p:post) = explore conn (p:pre) labels post
explore conn pre (l:labels) (p:post)    = commit conn l pre p post : explore conn (p:pre) labels post
explore conn _ _ _                      = []

commit :: Connected -> Label -> [Proc] -> Proc -> [Proc] -> (Label, [Proc])
commit conn l pre p post                = (l, commit' l pre (say l p ++ map (hear conn l) post))
  where commit' l [] post               = post
        commit' l (p:pre) post          = commit' l pre (hear conn l p : post)

may_say :: Proc -> Label
may_say (RInst (i,r) c ex (Enter x cont))             = ENTER (i,x)
may_say (RInst (i,r) c (x:ex) (Exit y cont)) | x==y   = EXIT  (i,x)
may_say (RInst (i,r) c ex (IrvRead s cont))           = IRVR  (i,s) void
may_say (RInst (i,r) c ex (IrvWrite s v cont))        = IRVW  (i,s) v
may_say (RInst (i,r) c ex (Receive e cont))           = RCV   (i,e) void
may_say (RInst (i,r) c ex (Send e v cont))            = SND   (i,e) v void
may_say (RInst (i,r) c ex (Read e cont))              = RD    (i,e) void
may_say (RInst (i,r) c ex (Write e v cont))           = WR    (i,e) v
may_say (RInst (i,r) c ex (IsUpdated e cont))         = UP    (i,e) void
may_say (RInst (i,r) c ex (Invalidate e cont))        = INV   (i,e)
may_say (RInst (i,r) c ex (Call o v cont))            = CALL  (i,o) v void
may_say (RInst (i,r) c ex (Result o cont))            = RES   (i,o) void
may_say (RInst (i,r) (Just a) ex (Terminate (Ok v)))  = RET   a v
may_say (RInst (i,r) Nothing [] (Terminate _))        = TERM  (i,r)
may_say (Run a 0.0 Pending n s)
  | n == 0 || invocation s == Concurrent              = NEW   a
may_say (Run a t act n s)                             = DELTA t
may_say (Timer a 0.0 t)                               = TICK  a
may_say (Timer a t t0)                                = DELTA t
may_say _                                             = PASS


say :: Label -> Proc -> [Proc]
say (ENTER _)      (RInst a c ex (Enter x cont))         = [RInst a c (x:ex) (cont void)]
say (EXIT _)       (RInst a c (_:ex) (Exit x cont))      = [RInst a c ex (cont void)]
say (IRVR _ res)   (RInst a c ex (IrvRead _ cont))       = [RInst a c ex (cont res)]
say (IRVW _ _)     (RInst a c ex (IrvWrite _ _ cont))    = [RInst a c ex (cont void)]
say (RCV _ res)    (RInst a c ex (Receive _ cont))       = [RInst a c ex (cont res)]
say (SND _ _ res)  (RInst a c ex (Send _ _ cont))        = [RInst a c ex (cont res)]
say (RD _ res)     (RInst a c ex (Read _ cont))          = [RInst a c ex (cont res)]
say (WR _ _)       (RInst a c ex (Write _ _ cont))       = [RInst a c ex (cont void)]
say (UP _ res)     (RInst a c ex (IsUpdated _ cont))     = [RInst a c ex (cont res)]
say (INV _)        (RInst a c ex (Invalidate _ cont))    = [RInst a c ex (cont void)]
say (CALL _ _ res) (RInst a c ex (Call o _ cont))        = [RInst a c ex (cont res)]
say (RES _ res)    (RInst a c ex (Result o cont))        = [RInst a c ex (cont res)]
say (RET _ _)      (RInst a _ ex (Terminate _))          = [RInst a Nothing ex (Terminate void)]
say (TERM _)       (RInst _ _ _ _)                       = []
say (NEW _)        (Run a _ Pending n s)                 = [Run a (minstart s) Idle (n+1) s,
                                                            RInst a Nothing [] (implementation s Void)]
say (NEW _)        (Run a _ (Serving (c:cs) (v:vs)) n s) = [Run a (minstart s) (Serving cs vs) (n+1) s,
                                                            RInst a (Just c) [] (implementation s v)]
say (DELTA d)      (Run a t act n s)                     = [Run a (t-d) act n s]
say (TICK _)       (Timer a _ t)                         = [Timer a t t]
say (DELTA d)      (Timer a t t0)                        = [Timer a (t-d) t0]


may_hear :: Connected -> Label -> Proc -> Label
may_hear conn PASS _                                            = PASS
may_hear conn (ENTER a)      (Excl b True)     | a==b           = ENTER a
may_hear conn (ENTER a)      (Excl b _)        | a==b           = PASS
may_hear conn (EXIT a)       (Excl b False)    | a==b           = EXIT a
may_hear conn (EXIT a)       (Excl b _)        | a==b           = PASS
may_hear conn (IRVR a res)   (Irv b v)         | a==b           = IRVR a (max res (Ok v))
may_hear conn (IRVW a v)     (Irv b _)         | a==b           = IRVW a v
may_hear conn (RCV a res)    (QElem b n (v:_)) | a==b           = RCV a (max res (Ok v))
may_hear conn (RCV a res)    (QElem b n [])    | a==b           = RCV a (max res NO_DATA)
may_hear conn (SND a v res)  (QElem b n vs)                    
        | a `conn` b && length vs < n                           = SND a v (max res void)
        | a `conn` b                                            = SND a v (max res LIMIT)
may_hear conn (SND a v res)  (Run _ _ _ _ s)   | trig conn a s  = SND a v (max res void)
may_hear conn (RD a res)     (DElem b u v)     | a==b           = RD a (max res v)
may_hear conn (WR a v)       (DElem b _ _)     | a `conn` b     = WR a v
may_hear conn (WR a v)       (Run _ _ _ _ s)   | trig conn a s  = WR a v
may_hear conn (UP a res)     (DElem b u _)     | a==b           = UP a (max res (Ok (VBool u)))
may_hear conn (INV a)        (DElem b _ _)     | a `conn` b     = INV a
may_hear conn (CALL a v res) (Run b t (Serving cs vs) n s)
        | trig conn a s && a `notElem` cs                       = CALL a v (max res void)
        | trig conn a s                                         = CALL a v (max res LIMIT)
may_hear conn (RES a res)    (Op b (v:vs))     | a==b           = RES a (max res (Ok v))
may_hear conn (RES a res)    (Op b [])         | a==b           = RES a (max res NO_DATA)
may_hear conn (RET a v)      (Op b vs)         | a==b           = RET a v
may_hear conn (TERM a)       (Run b _ _ _ _)   | a==b           = TERM a
may_hear conn (TICK a)       (Run b _ _ _ _)   | a==b           = TICK a
may_hear conn (DELTA d)      (Run _ t _ _ _)   | d <= t         = DELTA d
                                               | d > t          = PASS
may_hear conn (DELTA d)      (Timer _ t _)     | d <= t         = DELTA d
                                               | d > t          = PASS
may_hear conn label _                                           = label


hear :: Connected -> Label -> Proc -> Proc
hear conn (ENTER a)     (Excl b True)      | a==b               = Excl b False
hear conn (EXIT a)      (Excl b False)     | a==b               = Excl b True
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
hear conn (RES a _)     (Op b (v:vs))      | a==b               = Op b vs
hear conn (RES a _)     (Op b [])          | a==b               = Op b []
hear conn (RET a v)     (Op b vs)          | a==b               = Op b (vs++[v])
hear conn (TERM a)      (Run b t act n s)  | a==b               = Run b t act (n-1) s
hear conn (TICK a)      (Run b t _ n s)    | a==b               = Run b t Pending n s
hear conn (DELTA d)     (Run b t act n s)                       = Run b (t-d) act n s
hear conn (DELTA d)     (Timer b t t0)                          = Timer b (t-d) t0
hear conn label         proc                                    = proc

