{-# LANGUAGE FlexibleInstances, Rank2Types, ExistentialQuantification #-}

module ARSim where

import Data.Map ((!),size)
import ARText hiding (Exit)

data StdRet a   = Ok a
                | NO_DATA
                | NEVER_RECEIVED
                | LIMIT
                | UNCONNECTED
                | TIMEOUT
                | IN_EXCLUSIVE_AREA
                deriving (Eq,Ord,Show)

void :: StdRet Value
void            = Ok Void

type StdReturn  = StdRet Value

type Cont       = StdReturn -> Code

data Code       = Send PortName ElemName Value Cont
                | Receive PortName ElemName Cont
                | Write PortName ElemName Value Cont
                | Read PortName ElemName Cont
                | IsUpdated PortName ElemName Cont
                | Invalidate PortName ElemName Cont
                | Call PortName OpName Value Cont
                | Result PortName OpName Cont
                | IrvWrite VarName Value Cont
                | IrvRead VarName Cont
                | Enter ExclName Cont
                | Exit ExclName Cont
                | Terminate StdReturn
--  deriving (Eq, Ord, Show)
                
type Con a              = a -> Code

newtype RunM a          = RunM (Con a -> Code)

instance Monad RunM where
        RunM f >>= b    = RunM (\cont -> f (\x -> let RunM g = b x in g cont))
        return r        = RunM (\cont -> cont r)

runM                    :: (Valuable a, Valuable b) => (a -> RunM (StdRet b)) -> Value -> Code
runM m v                = let RunM f = m (fromVal v) in f (\r -> Terminate (toStd r))

-- Phatom type arguments to make the use more type safe
newtype RPE  c a        = RPE  (InstName, PortName, ElemName)
newtype PPE  c a        = PPE  (InstName, PortName, ElemName)
newtype RPO  c a b      = RPO  (InstName, PortName, OpName)
newtype PPO  c a b      = PPO  (InstName, PortName, OpName)
newtype IRV  c a        = IRV  (InstName, VarName)
newtype EXCL c          = EXCL (InstName, ExclName)

send                    :: Valuable a => PPE c a -> a -> RunM (StdRet ())
send (PPE (_,p,e)) v    = RunM (\cont -> Send p e (toVal v) (cont . fromStd))

receive                 :: Valuable a => RPE c a -> RunM (StdRet a)
receive (RPE (_,p,e))   = RunM (\cont -> Receive p e (cont . fromStd))

write                   :: Valuable a => PPE c a -> a -> RunM (StdRet ())
write (PPE (_,p,e)) v   = RunM (\cont -> Write p e (toVal v) (cont . fromStd))

read                    :: Valuable a => RPE c a -> RunM (StdRet a)
read (RPE (_,p,e))      = RunM (\cont -> Read p e (cont . fromStd))

isUpdated               :: Valuable a => RPE c a -> RunM (StdRet Bool)
isUpdated (RPE (_,p,e)) = RunM (\cont -> IsUpdated p e (cont . fromStd))

invalidate              :: Valuable a => PPE c a -> RunM (StdRet ())
invalidate (PPE (_,p,e)) = RunM (\cont -> Invalidate p e (cont . fromStd))

call                    :: (Valuable a, Valuable b) => RPO c a b -> a -> RunM (StdRet b)
call (RPO (_,p,o)) v    = RunM (\cont -> Call p o (toVal v) (cont . fromStd))

result                  :: (Valuable a, Valuable b) => RPO c a b -> RunM (StdRet b)
result (RPO (_,p,o))    = RunM (\cont -> Result p o (cont . fromStd))

irvWrite                :: Valuable a => IRV c a -> a -> RunM (StdRet ())
irvWrite (IRV (_,s)) v  = RunM (\cont -> IrvWrite s (toVal v) (cont . fromStd))

irvRead                 :: Valuable a => IRV c a -> RunM (StdRet a)
irvRead (IRV (_,s))     = RunM (\cont -> IrvRead s (cont . fromStd))

enter                   :: EXCL c -> RunM (StdRet ())
enter (EXCL (_,x))      = RunM (\cont -> Enter x (cont . fromStd))

exit                    :: EXCL c -> RunM (StdRet ())
exit (EXCL (_,x))       = RunM (\cont -> Exit x (cont . fromStd))

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

instance Functor StdRet where
        fmap f (Ok v)            = Ok (f v)
        fmap f NO_DATA           = NO_DATA
        fmap f NEVER_RECEIVED    = NEVER_RECEIVED
        fmap f LIMIT             = LIMIT
        fmap f UNCONNECTED       = UNCONNECTED
        fmap f TIMEOUT           = TIMEOUT
        fmap f IN_EXCLUSIVE_AREA = IN_EXCLUSIVE_AREA

toStd :: Valuable a => StdRet a -> StdReturn
toStd = fmap toVal

fromStd :: Valuable a => StdReturn -> StdRet a
fromStd = fmap fromVal

------------------------------------

type Conn               = ((InstName,Name,Name),(InstName,Name,Name))

type ProcessSoup = [P]

-- TODO: distinguish or name types for the Ints?
-- TODO: deriving Show etc. fails due to Cont being a function type
data State              = State {
                                inst    :: Int,
                                next    :: Int,
                                procs   :: ProcessSoup,
                                conns   :: [Conn]
                          }
startState :: State
startState = State 0 0 [] []


-- | A monad for building up the static structure. Accumulates the
-- ProcessSoup building up.
newtype AR c a          = AR (State -> (a,State))

instance Monad (AR c) where
        AR f >>= b      = AR (\s -> let (x,t) = f s; AR g = b x in g t)
        return x        = AR (\s -> (x,s))

runAR                   :: (forall c. AR c a) -> State -> (a, ProcessSoup, [Conn])
runAR (AR m) s          = let (r,t) = m s in (r, procs t, conns t)

get :: AR c State
get                     = AR (\s -> (s,s))

put :: State -> AR c ()
put s                   = AR (\_ -> ((),s))

newName :: AR c (Int, Int)
newName                 = do s <- get
                             put (s {next = next s + 1})
                             return (inst s,next s)

newName0 :: AR c (Int, Int, Int)
newName0                = do (i,n) <- newName
                             return (i,n,0)

addProc :: P -> AR c ()
addProc p               = do s <- get
                             put (s {procs = p:procs s})

addConn :: (InstName, Name, Name) -> 
           (InstName, Name, Name) -> 
           AR c ()
addConn a b             = do s <- get
                             put (s {conns = (a,b) : conns s})

requiredDataElement     :: AR c (RPE c a)
requiredDataElement     = do a <- newName0
                             addProc (DElem a False NO_DATA)
                             return (RPE a)

providedDataElement     :: AR c (PPE c a)
providedDataElement     = do a <- newName0
                             return (PPE a)

plainRunnable           :: (() -> RunM (StdRet ())) -> AR c ()
plainRunnable m         = do a <- newName
                             let imp = runM m
                             addProc (Run a 0.0 Idle 0)

serverRunnable          :: (Valuable a, Valuable b) => (a -> RunM (StdRet b)) -> AR c ()
serverRunnable m        = undefined

component               :: (forall c. AR c a) -> AR c a
component m             = do s <- get
                             let (r,ps,cs) = runAR m (s {inst = next s, next = 0})
                             put (s {next = next s + 1, procs = ps, conns = cs})
                             return r

connect                 :: PPE () a -> RPE () a -> AR c ()
connect (PPE a) (RPE b) = addConn a b

-- A hack to do some kind of subtype coercion. 
class Seal m where
        seal            :: m c a -> m () a

instance Seal RPE where
        seal (RPE a)    = RPE a

instance Seal PPE where
        seal (PPE a)    = PPE a


--------------------------------------------------------------------------------

r1                      :: PPE c Int -> () -> RunM (StdRet ())
r1 ppe arg              = do write ppe 123

c1 :: AR c (PPE () Int)
c1                      = do ppe <- providedDataElement
                             plainRunnable (r1 ppe)
                             return (seal ppe)

c2 :: AR c (PPE () a, RPE () a1)
c2                      = do p1 <- providedDataElement
                             p2 <- requiredDataElement
                             return (seal p1, seal p2)

tsst :: AR c ()
tsst                    = do ppe <- component c1
                             (p1,p2) <- component c2
                             connect ppe p2
                             



{-
             /- ->  qe1 - - CompB1
           /
   CompA  - - - ->  qe2 - - CompB2
           \  
            \ - ->  qe3 - - CompB3

   CompA1  - - \
                \
   CompA2  - - - ->  qe - - CompB
                /
   CompA3  - -/

   CompA1  - - op1 - - \
                        \
   CompA2  - - op2 - - - -> CompB
                        /
   CompA3  - - op3 - -/
-}

---------------------------------------

type Time       = Double

type Client     = (InstName, PortName, OpName)

data Act        = Idle
                | Pending
                | Serving [Client] [Value]
--  deriving (Eq, Ord, Show)

-- PJ: Why pairs and triples when there are newtypes RPE, IRV, etc. defined above?
data P          = Run   (InstName, RunName) Time Act Int
                | RInst (InstName, RunName) (Maybe Client) [ExclName] Code
                | Excl  (InstName, ExclName) Bool
                | Irv   (InstName, VarName) Value
                | Timer (InstName, RunName) Time Time
                | QElem (InstName, PortName, ElemName) Int [Value]
                | DElem (InstName, PortName, ElemName) Bool StdReturn
                | Op    (InstName, PortName, OpName) [Value]
--  deriving (Eq, Ord, Show)

data Label      = ENTER (InstName, ExclName)
                | EXIT  (InstName, ExclName)
                | IRVR  (InstName, VarName) StdReturn
                | IRVW  (InstName, VarName) Value
                | RCV   (InstName, PortName, ElemName) StdReturn
                | SND   (InstName, PortName, ElemName) Value StdReturn
                | RD    (InstName, PortName, ElemName) StdReturn
                | WR    (InstName, PortName, ElemName) Value
                | UP    (InstName, PortName, ElemName) StdReturn
                | INV   (InstName, PortName, ElemName)
                | CALL  (InstName, PortName, OpName) Value StdReturn
                | RES   (InstName, PortName, OpName) StdReturn
                | RET   (InstName, PortName, OpName) Value
                | NEW   (InstName, RunName)
                | TERM  (InstName, RunName)
                | TICK  (InstName, RunName)
                | DELTA Time
                | PASS
  deriving (Eq, Ord, Show)

-- This provides all possible results, a "scheduler" then needs to pick one.
step :: Package -> ProcessSoup -> [(Label, ProcessSoup)]
step m procs                            = explore m [] labels1 procs
  where labels0                         = map (may_say m) procs
        labels1                         = map (respond procs) labels0
        respond procs label             = foldl (may_hear m) label procs

explore :: Package -> [P] -> [Label] -> [P] -> [(Label, [P])]
explore m pre (PASS:labels) (p:post)    = explore m (p:pre) labels post
explore m pre (l:labels) (p:post)       = commit m l pre p post : explore m (p:pre) labels post
explore m _ _ _                         = []

commit :: Package -> Label -> [P] -> P -> [P] -> (Label, [P])
commit m l pre p post                   = (l, commit' l pre (say m l p ++ map (hear m l) post))
  where commit' l [] post               = post
        commit' l (p:pre) post          = commit' l pre (hear m l p : post)

may_say :: Package -> P -> Label
may_say m (RInst (i,r) c ex (Enter x cont))             = ENTER (i,x)
may_say m (RInst (i,r) c (x:ex) (Exit y cont)) | x==y   = EXIT  (i,x)
may_say m (RInst (i,r) c ex (IrvRead s cont))           = IRVR  (i,s) void
may_say m (RInst (i,r) c ex (IrvWrite s v cont))        = IRVW  (i,s) v
may_say m (RInst (i,r) c ex (Receive p e cont))         = RCV   (i,p,e) void
may_say m (RInst (i,r) c ex (Send p e v cont))          = SND   (i,p,e) v void
may_say m (RInst (i,r) c ex (Read p e cont))            = RD    (i,p,e) void
may_say m (RInst (i,r) c ex (Write p e v cont))         = WR    (i,p,e) v
may_say m (RInst (i,r) c ex (IsUpdated p e cont))       = UP    (i,p,e) void
may_say m (RInst (i,r) c ex (Invalidate p e cont))      = INV   (i,p,e)
may_say m (RInst (i,r) c ex (Call p o v cont))          = CALL  (i,p,o) v void
may_say m (RInst (i,r) c ex (Result p o cont))          = RES   (i,p,o) void
may_say m (RInst (i,r) (Just a) ex (Terminate (Ok v)))  = RET a v
may_say m (RInst (i,r) Nothing [] (Terminate _))        = TERM  (i,r)
may_say m (Run a 0.0 Pending n)
  | n == 0 || concurrent (runnable m a)                 = NEW a
may_say m (Run a t act n)                               = DELTA t
may_say m (Timer a 0.0 t)                               = TICK a
may_say m (Timer a t t0)                                = DELTA t
may_say m _                                             = PASS

sing :: a -> [a]
sing a = [a]

say :: Package -> Label -> P -> [P]
say m (ENTER _)      (RInst a c ex (Enter x cont))        = sing (RInst a c (x:ex) (cont void))
say m (EXIT _)       (RInst a c (_:ex) (Exit x cont))     = sing (RInst a c ex (cont void))
say m (IRVR _ res)   (RInst a c ex (IrvRead _ cont))      = sing (RInst a c ex (cont res))
say m (IRVW _ _)     (RInst a c ex (IrvWrite _ _ cont))   = sing (RInst a c ex (cont void))
say m (RCV _ res)    (RInst a c ex (Receive _ _ cont))    = sing (RInst a c ex (cont res))
say m (SND _ _ res)  (RInst a c ex (Send _ _ _ cont))     = sing (RInst a c ex (cont res))
say m (RD _ res)     (RInst a c ex (Read _ _ cont))       = sing (RInst a c ex (cont res))
say m (WR _ _)       (RInst a c ex (Write _ _ _ cont))    = sing (RInst a c ex (cont void))
say m (UP _ res)     (RInst a c ex (IsUpdated _ _ cont))  = sing (RInst a c ex (cont res))
say m (INV _)        (RInst a c ex (Invalidate _ _ cont)) = sing (RInst a c ex (cont void))
say m (CALL _ _ res) (RInst a c ex (Call p o _ cont))     
        | async m (p,o) a || res /= void                  = sing (RInst a c ex (cont res))
        | sync m (p,o) a                                  = sing (RInst a c ex (Result p o cont))
say m (RES _ res)    (RInst a c ex (Result p o cont))     = sing (RInst a c ex (cont res))
say m (RET _ _)      (RInst a _ ex (Terminate _))         = sing (RInst a Nothing ex (Terminate void))
say m (TERM _)       (RInst _ _ _ _)                      = []
say m (NEW _)        (Run a _ Pending n)                  = [Run a (minstart m a) Idle            (n+1),
                                                             RInst a Nothing [] (impl m a Void)]
say m (NEW _)        (Run a _ (Serving (c:cs) (v:vs)) n)  = [Run a (minstart m a) (Serving cs vs) (n+1),
                                                             RInst a (Just c) [] (impl m a v)]
say m (DELTA d)      (Run a t act n)                      = sing (Run a (t-d) act n)
say m (TICK _)       (Timer a _ t)                        = sing (Timer a t t)
say m (DELTA d)      (Timer a t t0)                       = sing (Timer a (t-d) t0)


may_hear :: Package -> Label -> P -> Label
may_hear m PASS _                                           = PASS
may_hear m (ENTER a)      (Excl b True)   | a==b            = ENTER a
may_hear m (ENTER a)      (Excl b _)      | a==b            = PASS
may_hear m (EXIT a)       (Excl b False)  | a==b            = EXIT a
may_hear m (EXIT a)       (Excl b _)      | a==b            = PASS
may_hear m (IRVR a res)   (Irv b v)       | a==b            = IRVR a (max res (Ok v))
may_hear m (IRVW a v)     (Irv b _)       | a==b            = IRVW a v
may_hear m (RCV a res)    (QElem b n (v:_)) | a==b          = RCV a (max res (Ok v))
may_hear m (RCV a res)    (QElem b n [])  | a==b            = RCV a (max res NO_DATA)
may_hear m (SND a v res)  (QElem b n vs)                    
        | connected m a b && length vs < n                  = SND a v (max res void)
        | connected m a b                                   = SND a v (max res LIMIT)
may_hear m (SND a v res)  (Run b _ _ _)   | trig m a b      = SND a v res
may_hear m (RD a res)     (DElem b u v)   | a==b            = RD a (max res v)
may_hear m (WR a v)       (DElem b _ _)   | connected m a b = WR a v
may_hear m (WR a v)       (Run b _ _ _)   | trig m a b      = WR a v
may_hear m (UP a res)     (DElem b u _)   | a==b            = UP a (max res (Ok (VBool u)))
may_hear m (INV a)        (DElem b _ _)   | connected m a b = INV a
may_hear m (CALL a v res) (Run b t (Serving cs vs) n)
        | trig m a b && a `notElem` cs                      = CALL a v (max res void)
        | trig m a b                                        = CALL a v (max res LIMIT)
may_hear m (RES a res)    (Op b (v:vs))   | a==b            = RES a (max res (Ok v))
may_hear m (RES a res)    (Op b [])       | a==b            = RES a (max res NO_DATA)
may_hear m (RET a v)      (Op b vs)       | a==b            = RET a v
may_hear m (TERM a)       (Run b t act n) | a==b            = TERM a
may_hear m (TICK a)       (Run b _ _ _)   | a==b            = TICK a
may_hear m (DELTA d)      (Run _ t _ _)   | d <= t          = DELTA d
                                          | d > t           = PASS
may_hear m (DELTA d)      (Timer _ t _)   | d <= t          = DELTA d
                                          | d > t           = PASS
may_hear m label _                                          = label


hear :: Package -> Label -> P -> P
hear m (ENTER a)     (Excl b True)      | a==b              = Excl b False
hear m (EXIT a)      (Excl b False)     | a==b              = Excl b True
hear m (IRVR a _)    (Irv b v)                              = Irv b v
hear m (IRVW a v)    (Irv b _)          | a==b              = Irv b v
hear m (RCV a _)     (QElem b n (v:vs)) | a==b              = QElem b n vs
hear m (RCV a _)     (QElem b n [])     | a==b              = QElem b n []
hear m (SND a v _)   (QElem b n vs) 
        | connected m a b && length vs < n                  = QElem b n (vs++[v])
        | connected m a b                                   = QElem b n vs
hear m (SND a _ _)   (Run b t _ n)      | trig m a b        = Run b t Pending n
hear m (RD a _)      (DElem b _ v)      | a==b              = DElem b False v
hear m (WR a v)      (DElem b _ _)      | connected m a b   = DElem b True (Ok v)
hear m (WR a _)      (Run b t _ n)      | trig m a b        = Run b t Pending n
hear m (UP a _)      (DElem b u v)      | a==b              = DElem b u v
hear m (INV a)       (DElem b _ _)      | connected m a b   = DElem b True NO_DATA
hear m (CALL a v _)  (Run b t (Serving cs vs) n)
        | trig m a b && a `notElem` cs                      = Run b t (Serving (cs++[a]) (vs++[v])) n
        | trig m a b                                        = Run b t (Serving cs vs) n
hear m (RES a _)     (Op b (v:vs))      | a==b              = Op b vs
hear m (RES a _)     (Op b [])          | a==b              = Op b []
hear m (RET a v)     (Op b vs)          | a==b              = Op b (vs++[v])
hear m (TERM a)      (Run b t act n)    | a==b              = Run b t act (n-1)
hear m (TICK a)      (Run b t _ n)      | a==b              = Run b t Pending n
hear m (DELTA d)     (Run b t act n)                        = Run b (t-d) act n
hear m (DELTA d)     (Timer b t t0)                         = Timer b (t-d) t0
hear m label         proc                                   = proc


connected :: Eq a => Package  -> (InstName, PortName, a) -> (InstName, PortName, a) -> Bool
connected m (i,p,e) (i',p',e')          = Connect (i,p) (i',p') `elem` connectors (comp m) && e == e'

trig :: Package -> (InstName, PortName, ElemName) -> (Int, RunName) -> Bool
trig m a b                              = or [ connected m a b | b <- elems ++ ops ]
  where ev                              = events (runnable m b)
        elems                           = [ (i,p,e) | DataReceivedEvent p e _ _ <- ev ]
        ops                             = [ (i,p,o) | OperationInvokedEvent p o _ _ <- ev ]
        (i,_,_)                         = a


async :: Package -> (PortName, OpNameOrStar) -> (Int, RunName) -> Bool
async m a b                             = fst a `elem` anyAsync || a `elem` async
  where cp                              = serverCallPoints (runnable m b)
        anyAsync                        = [ p | ServerCallPoint Asynchronous _ p 0 _ <- cp ]
        async                           = [ (p,o) | ServerCallPoint Asynchronous _ p o _ <- cp ]

sync :: Package -> (PortName, OpNameOrStar) -> (Int, RunName) -> Bool
sync m a b                              = fst a `elem` anySync || a `elem` sync
  where cp                              = serverCallPoints (runnable m b)
        anySync                         = [ p | ServerCallPoint Synchronous _ p 0 _ <- cp ]
        sync                            = [ (p,o) | ServerCallPoint Synchronous _ p o _ <- cp ]

minstart :: Package -> (Int, RunName) -> Double
minstart m a                            = minimumStartInterval (runnable m a)

-- ??
impl m a                                = undefined

comp :: Package -> Composition
comp m
  | size c == 1                         = c ! root m
  | otherwise                           = error "Non-flat component hierarchy"
  where c                               = compositions m

runnable :: Package -> (Int, RunName) -> Runnable
runnable m (i,r)                        = runnables beh ! r
  where Prototype c                     = subcomponents (comp m) ! i
        Application _ beh _             = components m ! c

