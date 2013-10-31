{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies,
             Rank2Types, ExistentialQuantification #-}

module ARSim -- Lets design the interface later...
{-
              (RunM, RE, PE, RQ, PQ(..), RO, PO, IV, EX, Value(..), Valuable(..), StdRet(..),
              rte_send, rte_receive, rte_write, rte_read, rte_isUpdated, rte_invalidate, 
              rte_call, rte_callAsync, rte_result, rte_irvWrite, rte_irvRead, rte_enter, rte_exit,
              AR, Time, Trigger(..), Invocation(..), component, runnable, serverRunnable,
              requiredDataElement, providedDataElement, requiredQueueElement, providedQueueElement,
              requiredOperation, providedOperation, interRunnableVariable, exclusiveArea, source, sink,
              Seal(..), seal2, seal3, seal4, seal5, seal6, seal7, sealAll, Tag, Taggable(..),
              Connect(..), connect2, connect3, connect4, connect5, connect6, connect7, connectAll,
              traceLabels, putTraceLabels, putTrace, simulationM, headSched, simulationHead, 
              simulationRerun, randSched, simulationRand, sendsTo) -} where

              
import Control.Monad (liftM)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Concurrent
import Data.List
import Data.Ord (comparing)
import Data.Tree
import Data.Function(on)

import System.Random (StdGen)
import Test.QuickCheck (Gen, elements, choose, shrinkNothing, forAllShrink, sized, Property)
import Test.QuickCheck.Arbitrary (shrinkList)
import Test.QuickCheck.Gen (Gen(..))
import qualified Control.Monad.Writer as W
import qualified Control.Monad.State as S

-- | A monad for writing runnables
newtype RunM a          = RunM (Con a -> Code)

instance Functor RunM where
        fmap f (RunM g) = RunM (\cont -> g (cont . f))
        
instance Monad RunM where
        RunM f >>= b    = RunM (\cont -> f (\x -> let RunM g = b x in g cont))
        return r        = RunM (\cont -> cont r)

-- Phantom type arguments to make the use more type safe
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
                deriving (Eq,Ord)

instance Show Value where
        show (Void)             = "()"
        show (VBool v)          = show v
        show (VInt v)           = show v
        show (VReal v)          = show v
        show (VString v)        = show v
        show (VArray v)         = show v

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

rte_send        :: Valuable a => PQ a c -> a -> RunM (StdRet ())
-- Non-blocking receive. Returns NO_DATA if the queue is empty
rte_receive     :: Valuable a => RQ a c -> RunM (StdRet a)
rte_write       :: Valuable a => PE a c -> a -> RunM (StdRet ())
rte_read        :: Valuable a => RE a c -> RunM (StdRet a)
rte_isUpdated   :: Valuable a => RE a c -> RunM (StdRet Bool)
rte_invalidate  :: Valuable a => PE a c -> RunM (StdRet ())
rte_call        :: (Valuable a, Valuable b) => RO a b c -> a -> RunM (StdRet b)
rte_callAsync   :: (Valuable a, Valuable b) => RO a b c -> a -> RunM (StdRet ())
rte_result      :: (Valuable a, Valuable b) => RO a b c -> RunM (StdRet b)
rte_irvWrite    :: Valuable a => IV a c -> a -> RunM (StdRet ())
rte_irvRead     :: Valuable a => IV a c -> RunM (StdRet a)
-- Enter is blocking
rte_enter       :: EX c -> RunM (StdRet ())
rte_exit        :: EX c -> RunM (StdRet ())


-- A monad for building up the static structure. Accumulates the
-- ProcessSoup building up.
newtype AR c a          = AR (State -> (a,State))

instance Functor (AR c) where
        fmap f (AR g)   = AR (\s -> let (x,s') = g s in (f x, s'))
        
instance Monad (AR c) where
        AR f >>= b      = AR (\s -> let (x,t) = f s; AR g = b x in g t)
        return x        = AR (\s -> (x,s))

type Time               = Double

data Trigger c          = forall a. ReceiveE (RE a c)
                        | forall a. ReceiveQ (RQ a c)
                        | Timed Time
                        | Init

data Invocation         = Concurrent
                        | MinInterval Time
                        deriving (Eq,Show)


requiredDataElement     :: AR c (RE a c)
requiredDataElementInit :: Valuable a => a -> AR c (RE a c)
providedDataElement     :: AR c (PE a c)
-- The queue is physically located on the 'required' side,
-- which is why the receiver specifies the max queue length.
requiredQueueElement    :: Int -> AR c (RQ a c)
providedQueueElement    :: AR c (PQ a c)
-- Operations are remote procedure calls
requiredOperation       :: AR c (RO a b c)
providedOperation       :: AR c (PO a b c)
interRunnableVariable   :: Valuable a => a -> AR c (IV a c)
exclusiveArea           :: AR c (EX c)
runnable                :: Invocation -> [Trigger c] -> RunM a -> AR c ()
serverRunnable          :: (Valuable a, Valuable b) => 
                           Invocation -> [PO a b c] -> (a -> RunM b) -> AR c ()

component               :: (forall c. AR c a) -> AR c a

source                  :: (Valuable a) => [(Time,a)] -> AR c (PE a ())
sink                    :: (Valuable a) => AR c (RE a ())


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

sealAll xs                      = map seal xs

seal2 (a1,a2)                   = (seal a1, seal a2)
seal3 (a1,a2,a3)                = (seal a1, seal a2, seal a3)
seal4 (a1,a2,a3,a4)             = (seal a1, seal a2, seal a3, seal a4)
seal5 (a1,a2,a3,a4,a5)          = (seal a1, seal a2, seal a3, seal a4, seal a5)
seal6 (a1,a2,a3,a4,a5,a6)       = (seal a1, seal a2, seal a3, seal a4, seal a5, seal a6)
seal7 (a1,a2,a3,a4,a5,a6,a7)    = (seal a1, seal a2, seal a3, seal a4, seal a5, seal a6, seal a7)

class Connect a b | a -> b, b -> a where
        connect                 :: a -> b -> AR c ()

instance Connect (PE a ()) (RE a ()) where
        connect (PE a) (RE b)   = addConn (a,b)
        
instance Connect (PQ a ()) (RQ a ()) where
        connect (PQ a) (RQ b)   = addConn (a,b)
        
instance Connect (RO a b ()) (PO a b ()) where
        connect (RO a) (PO b)   = addConn (a,b)
        
connectAll a b                  = mapM (uncurry connect) (a `zip` b)
        
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

newtype Tag                     = Tag Name2
                                    -- deriving Show -- Probably we want to remove this instance later, but it's useful for debugging

class Taggable a where
        tag                     :: [a] -> [Tag]

instance Taggable (PE a ()) where
        tag xs                  = [ Tag n | PE n <- xs ]

instance Taggable (RE a ()) where
        tag xs                  = [ Tag n | RE n <- xs ]
        
instance Taggable (PQ a ()) where
        tag xs                  = [ Tag n | PQ n <- xs ]

instance Taggable (RQ a ()) where
        tag xs                  = [ Tag n | RQ n <- xs ]
        
instance Taggable (PO a b ()) where
        tag xs                  = [ Tag n | PO n <- xs ]

instance Taggable (RO a b ()) where
        tag xs                  = [ Tag n | RO n <- xs ]
        
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

instance Show Cont where
  show _ = "_"

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
  deriving (Show)
                
--instance Show Code where
--        show (Send a v _)       = spacesep ["Send", show a, show v]

type Con a              = a -> Code


rte_send (PQ (_,e)) v       = RunM (\cont -> Send e (toVal v) (cont . fromStd))

rte_receive (RQ (_,e))      = RunM (\cont -> Receive e (cont . fromStd))

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

runAR                   :: (forall c. AR c a) -> State -> (a, State)
runAR (AR m) s          = m s

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
                             let (r,s') = runAR m (s {inst = next s, next = next s + 1})
                             put (s' {inst = inst s})
                             return r

runnable inv tr m       = do a <- newName
                             mapM (addProc . Timer a 0.0) ts
                             addProc (Run a 0.0 act 0 (static inv ns cont))
        where ns        = [ n | ReceiveE (RE n) <- tr ] ++ [ n | ReceiveQ (RQ n) <- tr ]
              ts        = [ t | Timed t <- tr ]
              act       = if null [ Init | Init <- tr ] then Idle else Pending
              cont _    = let RunM f = m in f (\_ -> Terminate void)

serverRunnable inv tr m = do a <- newName
                             addProc (Run a 0.0 (Serving [] []) 0 (static inv ns cont))
        where ns        = [ n | PO n <- tr ]
              cont v    = let RunM f = m (fromVal v) in f (\r -> Terminate (Ok (toVal r)))

requiredDataElement     = do a <- newName
                             addProc (DElem a False NO_DATA)
                             return (RE a)

requiredDataElementInit v
                        = do a <- newName
                             addProc (DElem a False (Ok (toVal v)))
                             return (RE a)

providedDataElement     = do a <- newName
                             return (PE a)

requiredQueueElement n  = do a <- newName
                             addProc (QElem a n [])
                             return (RQ a)

providedQueueElement    = do a <- newName
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

source vs               = do a <- newName
                             addProc (Src a (relative 0.0 [(t,toVal v)|(t,v)<-vs]))
                             return (PE a)
  where relative t0 []  = []
        relative t0 ((t,v):pts)     
                        = (t-t0,v) : relative t pts


sink                    = do a <- newName
                             addProc (Sink a 0.0 [])
                             return (RE a)

type Client     = (InstName, OpName)

-- Activation
-- Current activation state
data Act        = Idle -- Not activated
                | Pending -- Ready to be run
                | Serving [Client] [Value] -- Only applies to server runnables.
                -- If an operation is called while its runnable is running, it is queued.
                -- Server runnables are always in this state.
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
                | RInst (InstName, RunName) Int (Maybe Client) [ExclName] Code
                | Excl  (InstName, ExclName) Bool
                | Irv   (InstName, VarName) Value
                | Timer (InstName, RunName) Time Time
                | QElem (InstName, ElemName) Int [Value]
                | DElem (InstName, ElemName) Bool StdReturn
                | Op    (InstName, OpName) [Value]
                | Src   (InstName, ElemName) [(Time,Value)]
                | Sink  (InstName, ElemName) Time [(Time,Value)]

spacesep        = concat . intersperse " " -- JD: Same as unwords from prelude?

instance Show Proc where
        show (Run a t act n s)  = spacesep ["Run", show a, show t, show act, show n]
        show (RInst a n c ex d) = spacesep ["RInst", show a, show n, show c, show ex, show d]
        show (Excl a v)         = spacesep ["Excl", show a, show v]
        show (Irv a v)          = spacesep ["Irv", show a, show v]
        show (Timer a v t)      = spacesep ["Timer", show v, show t]
        show (QElem a n vs)     = spacesep ["QElem", show a, show n, show vs]
        show (DElem a v r)      = spacesep ["DElem", show a, show v, show r]
        show (Op a vs)          = spacesep ["Op", show a, show vs]
        show (Src a vs)         = spacesep ["Src", show a, show vs]
        show (Sink a t vs)      = spacesep ["Sink", show a, show t, show vs]

showTV vs = "[" ++ unwords [ "(" ++ show (round (1000*t)) ++ ", " ++ show v ++ ")" | (t,v) <- vs ] ++ "]"

shortProc :: Proc -> String
shortProc (Run a t act n s)  = spacesep ["Run", show a]
shortProc (RInst a n c _ _)  = spacesep ["RInst", show a++"-"++show n]
shortProc (Excl a v)         = spacesep ["Excl", show a]
shortProc (Irv a v)          = spacesep ["Irv", show a]
shortProc (Timer a v t)      = spacesep ["Timer", show a]
shortProc (QElem a n vs)     = spacesep ["QElem", show a]
shortProc (DElem a v r)      = spacesep ["DElem", show a]
shortProc (Op a vs)          = spacesep ["Op", show a]
shortProc (Src a vs)         = spacesep ["Src", show a]
shortProc (Sink a t vs)      = spacesep ["Sink", show a]

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
labelName :: Label -> Maybe (Name,Name)
labelName (SND n _ _) = Just n
labelName _           = Nothing




-- An initial state, and all intermediate states
-- JD: Maybe the initial state is not even needed, it can be recomputed easily for a given program.
type Trace = ([ParentProc], [SchedulerOption])
type ParentProc = (Proc,(Int,Int)) -- The first Int is an index in the trace (the parent)
                                   -- The second is which child of the parent this is, with 0
                                   --   signifying that it runs in the same thread as the parent.

-- The list is not empty. 
-- The single process in each choice is the one speaking.
-- The Int is the number of choices made so far (not really needed perhaps).
type SchedulerOption    = (Label, (ParentProc, [ParentProc]))
type SchedulerM m       = Int -> [SchedulerOption] -> m (Maybe SchedulerOption)

orphan :: ParentProc -> Proc
orphan (p,_) = p
orphans      = map orphan

adopt :: Int -> Proc -> ParentProc
adopt i p = (p,(i,0))
adopts :: Int -> [Proc] -> [ParentProc]
adopts i ps = [(p,(i,k)) | (p,k) <- zip ps [0..]]

siblingTo :: ParentProc -> ParentProc -> Bool
(_,x) `siblingTo` (_,y) = x == y -- This is more like "identical twins" than siblings.

processParent :: ParentProc -> (Int,Int)
processParent (_,x) = x

setProcessParent :: ParentProc -> (Int,Int) -> ParentProc
setProcessParent (x,_) p = (x,p)

optionSpeaker :: SchedulerOption -> ParentProc
optionSpeaker (_,(p,_)) = p

setOptionSpeaker :: SchedulerOption -> ParentProc -> SchedulerOption
setOptionSpeaker (x,(_,y)) p = (x,(p,y))

optionParent :: SchedulerOption -> (Int,Int)
optionParent = processParent . optionSpeaker

setOptionParent :: SchedulerOption -> (Int,Int) -> SchedulerOption
setOptionParent so = setOptionSpeaker so . setProcessParent (optionSpeaker so)

optionLabel :: SchedulerOption -> Label
optionLabel = fst

opt `firstBornOf` k = optionParent opt == (k,0)
opt `childOf` k     = fst (optionParent opt) == k




traceLabels :: Trace -> [Label]
traceLabels (_,xs) = map fst xs

traceLabelParents :: Trace -> [(Int, (Int,Int), Label)]
traceLabelParents (_,xs) = map (\(ix,(l,((_,x),_))) -> (ix,x,l)) (zip [0..] xs)

traceSteps :: Trace -> [SchedulerOption]
traceSteps = snd

finalState :: Trace -> [Proc]
finalState (_,xs) = orphans $ snd $ snd $ last xs

putTrace, putTraceLabels :: Trace -> IO ()
-- Outputs the labels and the final state
putTrace x = putTraceLabels x >> print (finalState x)

putTraceLabels = mapM_ printLabel . traceLabels where
  printLabel l@(DELTA t) = print l >> threadDelay (round (t*1000000))
  printLabel l           = print l

putTraceAll :: Trace -> IO ()
putTraceAll = mapM_ printAll . snd
  where printAll (l,(_,ps)) = print l >> print (map fst ps)

putTraceForest :: Trace -> IO ()
putTraceForest = putStrLn . drawForest . map (fmap showNode) . toForest

type SchedOpt' = (Int,ParentProc,Label)

toForest :: Trace -> Forest SchedOpt'
toForest (ps,tc)= toForest' (-1) indexed where
  indexed = (zip tc [0..])
  toForest' :: Int -> [(SchedulerOption,Int)] -> Forest SchedOpt'
  toForest' par sos = map snd $ sortBy (compare `on` fst)
    [(snd (optionParent so),toTree ix so)|(so,ix) <- sos, fst (optionParent so) == par]
  toTree :: Int -> SchedulerOption -> Tree SchedOpt'
  toTree ix so = Node (ix, optionSpeaker so, optionLabel so) (toForest' ix indexed) 
                              -- could drop ix from indexed to optimise 

showNode :: SchedOpt' -> String
showNode (ix,p,l) = (star $ snd $ snd p) ++ show ix ++ ": " ++ shortProc (orphan p) ++ " !!! " ++ show l
  where star 0 = ""; star _ = "*"

dfsM :: Monad m => (a -> m a') -> (a' -> [b] -> m b) -> Tree a -> m b
dfsM e f (Node {rootLabel = l, subForest = s}) = do
  x <- e l
  xs <- mapM (dfsM e f) s
  f x xs

allocatePositions :: Monad m => (a -> m i) -> Tree a -> m (Tree (a, i))
allocatePositions c = dfsM e f
  where e x = do y <- c x
                 return (x, y)
        f l sf = return $ Node { rootLabel = l, subForest = sf }

reallyAllocate :: Tree SchedOpt' -> S.State Int (Tree (SchedOpt', Int))
reallyAllocate = allocatePositions c
  where c x@(_, p, _) = do
             i <- S.get
             let fc = snd $ snd p
                 bump 0 = 0
                 bump _ = 1
                 j = bump fc + i
             S.put j
             return j

reallyAllocateF :: Forest SchedOpt' -> S.State Int (Forest (SchedOpt', Int))
reallyAllocateF = mapM reallyAllocate

byRows :: Forest (SchedOpt', Int) -> [(SchedOpt', Int)]
byRows = sortBy (comparing cmpExtr) . flattenF
  where
  cmpExtr ((row, _, _), _col) = row

flattenF = concatMap flatten

printRow :: Int -> Int -> (Int -> String) -> String
printRow width tot prt =
  intercalate " | " [ take width $ prt i ++ repeat ' ' | i <- [0..tot-1]] 

printTraceRow :: (SchedOpt', Int) -> Int -> String
printTraceRow ((_row, _, lab), col) i
  | col == i + 1 = show lab
  | otherwise    = ""

traceTable :: Trace -> String
traceTable t = unlines $ map (printRow 10 lind . printTraceRow) $ byRows f
  where
  (f, lind) = S.runState (mapM reallyAllocate $ toForest t) 0

putTraceTable = putStr . traceTable
sendsTo :: [Tag] -> Trace -> [Value]
sendsTo tags (_,ls) = [ v | SND a v _ <- map fst ls, a `elem` ns ]
  where ns = [ n | Tag n <- tags ]

writesTo :: [Tag] -> Trace -> [Value]
writesTo tags (_,ls) = [ v | WR a v <- map fst ls, a `elem` ns ]
  where ns = [ n | Tag n <- tags ]

headSched :: SchedulerM Identity
headSched _ = return . Just . head

simulationHead :: (forall c. AR c a) -> (Trace, a)
simulationHead m = (runIdentity m', a) 
  where (m', a) = simulationM headSched m

maximumProgress :: SchedulerM m -> SchedulerM m
maximumProgress sched n opts
  | null work = sched n deltas
  | otherwise = sched n work
  where (deltas,work) = partition isDelta opts
        isDelta (DELTA _, _) = True
        isDelta _            = False

randSched :: SchedulerM Gen
randSched _ = fmap Just . elements

simulationRandG :: (forall c. AR c a) -> Gen (Trace, a)
simulationRandG m = do
  let (g, a) = simulationM randSched m
  x <- g
  return (x,a)

simulationRand :: StdGen -> (forall c. AR c a) -> (Trace, a)
simulationRand rng m = (unGen g rng 0, a)
  where (g, a) = simulationM (maximumProgress randSched) m



-- Scheduler that reruns a trace. The only difficult part is what to do if there are several
--   labels in the choice list (ls) that match the next label from the rerun trace.
rerunSched :: SchedulerM (S.State Trace)
rerunSched n ls = do
  (init,steps) <- S.get
  case steps of 
    []         -> return Nothing -- Terminate
    (rr,(gpar,_)):rrs'  -> do 
      S.put (init, rrs')
      case [x | x@(lab, (gparx,_)) <- ls, (lab `similarLabel` rr) && (gpar `siblingTo` gparx)] of
        []     -> do
           let rrs'' = shortCut n (snd gpar) rrs'
           S.put (init,rrs'')
           rerunSched n ls
        [x]    -> return $ Just $ x
        (x:xs) -> return $ Just $ x -- Take the label that's closest to the original choice
        
similarLabel :: Label -> Label -> Bool
similarLabel (IRVR n1 _)   (IRVR n2 _)   = n1 == n2
similarLabel (IRVW n1 _)   (IRVW n2 _)   = n1 == n2
similarLabel (RES n1 _)    (RES n2 _)    = n1 == n2
similarLabel (RET n1 _)    (RET n2 _)    = n1 == n2
similarLabel (RCV n1 _)    (RCV n2 _)    = n1 == n2
similarLabel (SND n1 _ _)  (SND n2 _ _)  = n1 == n2
-- Several more cases could be added.
similarLabel a             b             = a == b

simulationRerun :: Trace -> (forall c. AR c a) -> (Trace, a)
simulationRerun tc m = (S.evalState m' tc, a)
    where (m',a) = simulationM rerunSched m


    
-- Shrinking and property stuff    

-- A simulation trace along with a list of ports to observe
newtype Sim = Sim (Trace, [Tag])
simTrace (Sim (t,_)) = t

instance Show Sim where
--  show (Sim (t,_)) = unlines $ map show $ sortByParent $ traceLabelParents t
  show x = unlines [simForest x, simTable x]

simForest :: Sim -> String
simForest x = drawForest (map (fmap showNode) $ toForest $ simTrace x)

simTable :: Sim -> String
simTable (Sim (t, _)) = traceTable t
                             
sortByParent tls = sortBy (compare `on` (\(a,b,c) -> b)) tls

traceProp :: (forall c. AR c [Tag]) -> (Sim -> Bool) -> Property
traceProp code prop = sized $ \n -> do
  let limit = (1+n*10)
      gen :: Gen Sim
      gen = fmap (cutSim limit . Sim) $ simulationRandG code
  -- forAllShrink gen shrinkNothinh prop -- Disable shrinking
  forAllShrink gen (shrinkSim code) prop



-- Take a finite initial part of the simulation trace
cutSim :: Int -> Sim -> Sim
cutSim n (Sim (t,xs)) = Sim (fmap (take n) t, xs)

-- Shrink the simulation trace and rerun it
-- This would work much better if the trace was a tree, branching on new processes.
shrinkSim :: (forall c. AR c [Tag]) -> Sim -> [Sim]
shrinkSim code (Sim (trc,_)) = [rerun tn'| tn' <- shrink2 shrinkTrace trc ] where
  rerun tns = Sim $ simulationRerun tns code

shrink2 shrnk x =
    [ y | y <- shrnk_x ] ++
    [ z
    | y <- shrnk_x
    , z <- shrnk y
    ]
   where
    shrnk_x = shrnk x 

shrinkTrace :: Trace -> [Trace]
shrinkTrace (init,t) =  -- [(init,t')|t' <- removeSingle 0 t]
                     [(init,t')|t' <- cutLoop 0 t] ++
                     [(init,t')|t' <- sequentialise 0 t]

cutLoop _ []     = []
cutLoop _ [x]    = [[]]
cutLoop k (x:xs) = shortCut k (optionParent x) xs : map (x:) (cutLoop (k+1) xs)

shortCut :: Int -> (Int,Int) -> [SchedulerOption] -> [SchedulerOption]
shortCut ds p []     = []
shortCut ds p ((r@(x1,((x2,(n,c)),x4))):xs) = if n == ds
  then (x1,((x2,p),x4)) : shortCut ds p xs
  else (if ds <= n then (x1,((x2,(n-1,c)),x4)) else r) : shortCut ds p xs

removeSingle _ [] = []
removeSingle _ [x] = [[]]
removeSingle k (x:xs) = cascade (k+1) [k] xs : map (x:) (removeSingle (k+1) xs)

cascade :: Int -> [Int] -> [SchedulerOption] -> [SchedulerOption]
cascade k ds []     = []
cascade k ds ((r@(x1,((x2,(n,x3)),x4))):xs) = if n `elem` ds 
  then cascade (k+1) (ds ++ [k]) xs
  else let skips = length (takeWhile (< n) ds) in 
    (x1,((x2,(n-skips,x3)),x4)) : cascade (k+1) ds xs

-- Try to move sequential broadcasts closer to their parents
sequentialise k (x:y:xys) = fmap (x:) $ if yp == k && yc == 0
  then rc
  else maybe id (\(e,_,es) -> ((e:y:es):)) (goFish (k+1) xys) $ rc
  where  
    rc = sequentialise (k+1) (y:xys)
    (yp, yc) = optionParent y
    -- Look for a 'firstborn' child of x, and return it along with it's index an updated list
    goFish :: Int -> [SchedulerOption] -> Maybe (SchedulerOption, Int, [SchedulerOption])
    goFish m []           = Nothing
    goFish m (x:xs)
      | x `firstBornOf` k = Just (x, m, map (updateRef m) xs) 
      | otherwise         = fmap (\(e,m',es) -> (e,m',updateRef m' x:es)) (goFish (m+1) xs)
      where
        updateRef m x
          | op == m                   = updateIndex x (const (k+1))
          | op < m && op > k          = updateIndex x (+1)
          | otherwise                 = x
          where op = fst (optionParent x) 
        updateIndex x f = setOptionParent x (f ix,c)
          where (ix,c) = optionParent x
sequentialise k _         = []




chopTrace :: Int -> (Trace,a) -> (Trace,a)
chopTrace n ((x,tr),a) = ((x, take n tr),a)

collect :: Valuable a => Trace -> (RE a c) -> [(Time,a)]
collect trace (RE a)    = case [ vs | Sink b _ vs <- procs, a==b ] of
                                [] -> []
                                (vs:_) -> absolute 0.0 ([ (t, fromVal v) | (t,v) <- reverse vs ])
  where schopts         = snd trace
        final           = last schopts
        pprocs          = snd (snd final)
        procs           = map fst pprocs
        absolute t0 []  = []
        absolute t0 ((t,v):pts)
                        = (t0+t,v) : absolute (t0+t) pts


simulationM :: Monad m => SchedulerM m -> (forall c. AR c a) -> (m Trace, a)
simulationM sched m     = (liftM (\ss -> (procs_s, ss)) traceM, tags)
  where (tags,s)        = runAR m startState
        s1              = initSources s
        traceM          = simulateM sched (connected (conns s1)) procs_s
        procs_s         = zip (procs s1) $ map (\x -> (-1,x)) [1..]

simulateM :: Monad m => SchedulerM m -> Connected -> [ParentProc] -> m [SchedulerOption]
simulateM = go 0 where
  go k sched conn procs
    | null next           = return []
    | otherwise           = do
      maysched <- sched k next
      case maysched of
        Nothing               -> return []
        Just trans@(l,(_,procs')) -> liftM (\plss -> trans:plss) $ go (k+1) sched conn procs'
    where next            = step conn procs k

-- This provides all possible results, a "scheduler" then needs to pick one.
step :: Connected -> [ParentProc] -> Int -> [SchedulerOption]
step conn procs k                       = explore conn [] labels1 procs k
  where labels0                         = map may_say (orphans procs)
        labels1                         = map respond labels0
        respond label                   = foldl (may_hear conn) label (orphans procs)

explore :: Connected -> [ParentProc] -> [Label] -> [ParentProc] -> Int -> [SchedulerOption]
explore conn pre (PASS:labels) (p:post) k = explore conn (p:pre) labels post k
explore conn pre (l:labels)    (p:post) k = commit conn l pre p post k : explore conn (p:pre) labels post k
explore conn _ _ _ _                      = []

commit :: Connected -> Label -> [ParentProc] -> ParentProc -> [ParentProc] -> Int -> SchedulerOption
commit conn l pre p post k       = (l, (p, commit' l pre (adopts k (say l $ orphan p) ++ map (hear conn l k) post)))
  where commit' l [] post        = post
        commit' l (p:pre) post   = commit' l pre (hear conn l k p : post)

may_say :: Proc -> Label
may_say (RInst (i,r) _ c ex (Enter x cont))             = ENTER (i,x)
may_say (RInst (i,r) _ c (x:ex) (Exit y cont)) | x==y   = EXIT  (i,x)
may_say (RInst (i,r) _ c ex (IrvRead s cont))           = IRVR  (i,s) void
may_say (RInst (i,r) _ c ex (IrvWrite s v cont))        = IRVW  (i,s) v
may_say (RInst (i,r) _ c ex (Receive e cont))           = RCV   (i,e) void
may_say (RInst (i,r) _ c ex (Send e v cont))            = SND   (i,e) v void
may_say (RInst (i,r) _ c ex (Read e cont))              = RD    (i,e) void
may_say (RInst (i,r) _ c ex (Write e v cont))           = WR    (i,e) v
may_say (RInst (i,r) _ c ex (IsUpdated e cont))         = UP    (i,e) void
may_say (RInst (i,r) _ c ex (Invalidate e cont))        = INV   (i,e)
may_say (RInst (i,r) _ c ex (Call o v cont))            = CALL  (i,o) v void
may_say (RInst (i,r) _ c ex (Result o cont))            = RES   (i,o) void
may_say (RInst (i,r) _ (Just a) ex (Terminate (Ok v)))  = RET   a v
may_say (RInst (i,r) _ Nothing [] (Terminate _))        = TERM  (i,r)
may_say (Run a 0.0 Pending n s)
  | n == 0 || invocation s == Concurrent              = NEW   a
may_say (Run a 0.0 (Serving (c:cs) (v:vs)) n s)
  | n == 0 || invocation s == Concurrent              = NEW   a
may_say (Run a t act n s) | t > 0.0                   = DELTA t
may_say (Timer a 0.0 t)                               = TICK  a
may_say (Timer a t t0) | t > 0.0                      = DELTA t
may_say (Src a ((0.0,v):vs))                          = WR    a v
may_say (Src a ((t,v):vs)) | t > 0.0                  = DELTA t
may_say _                                             = PASS


say :: Label -> Proc -> [Proc]
say (ENTER _)      (RInst a n c ex (Enter x cont))       = [RInst a n c (x:ex) (cont void)]
say (EXIT _)       (RInst a n c (_:ex) (Exit x cont))    = [RInst a n c ex (cont void)]
say (IRVR _ res)   (RInst a n c ex (IrvRead _ cont))     = [RInst a n c ex (cont res)]
say (IRVW _ _)     (RInst a n c ex (IrvWrite _ _ cont))  = [RInst a n c ex (cont void)]
say (RCV _ res)    (RInst a n c ex (Receive _ cont))     = [RInst a n c ex (cont res)]
say (SND _ _ res)  (RInst a n c ex (Send _ _ cont))      = [RInst a n c ex (cont res)]
say (RD _ res)     (RInst a n c ex (Read _ cont))        = [RInst a n c ex (cont res)]
say (WR _ _)       (RInst a n c ex (Write _ _ cont))     = [RInst a n c ex (cont void)]
say (UP _ res)     (RInst a n c ex (IsUpdated _ cont))   = [RInst a n c ex (cont res)]
say (INV _)        (RInst a n c ex (Invalidate _ cont))  = [RInst a n c ex (cont void)]
say (CALL _ _ res) (RInst a n c ex (Call o _ cont))      = [RInst a n c ex (cont res)]
say (RES _ res)    (RInst a n c ex (Result o cont))      = [RInst a n c ex (cont res)]
say (RET _ _)      (RInst a n _ ex (Terminate _))        = [RInst a n Nothing ex (Terminate void)]
say (TERM _)       (RInst _ _ _ _ _)                     = []
say (NEW _)        (Run a _ Pending n s)                 = [Run a (minstart s) Idle (n+1) s,
                                                            RInst a n Nothing [] (implementation s Void)]
say (NEW _)        (Run a _ (Serving (c:cs) (v:vs)) n s) = [Run a (minstart s) (Serving cs vs) (n+1) s,
                                                            RInst a n (Just c) [] (implementation s v)]
say (DELTA d)      (Run a t act n s)                     = [Run a (t-d) act n s]
say (TICK _)       (Timer a _ t)                         = [Timer a t t]
say (DELTA d)      (Timer a t t0)                        = [Timer a (t-d) t0]
say (WR _ _)       (Src a (_:vs))                        = [Src a vs]
say (DELTA d)      (Src a ((t,v):vs))                    = [Src a ((t-d,v):vs)]



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
--may_hear conn (RES a res)    (Op b [])         | a==b           = RES a (max res NO_DATA)
may_hear conn (RES a res)    (Op b [])         | a==b           = PASS
may_hear conn (RET a v)      (Op b vs)         | a==b           = RET a v
may_hear conn (TERM a)       (Run b _ _ _ _)   | a==b           = TERM a
may_hear conn (TICK a)       (Run b _ _ _ _)   | a==b           = TICK a
may_hear conn (DELTA d)      (Run _ t _ _ _)   | t == 0         = DELTA d
                                               | d <= t         = DELTA d
                                               | d > t          = PASS
may_hear conn (DELTA d)      (Timer _ t _)     | d <= t         = DELTA d
                                               | d > t          = PASS
may_hear conn (DELTA d)      (Src a ((t,v):_)) | d <= t         = DELTA d
                                               | d > t          = PASS
may_hear conn (WR a v)       (Sink b _ _)      | a `conn` b     = WR a v
may_hear conn (DELTA d)      (Sink _ _ _)                       = DELTA d
may_hear conn label _                                           = label


-- Not sure if a change here should actually cause adopt or not, currently it does not.
-- Probably it should not. The process-parent (and the scheduler as a whole) only keep tracks of who speaks.
hear :: Connected -> Label -> Int -> ParentProc -> ParentProc
hear conn l par pp = maybe pp (\p' -> (p',processParent pp)) (hear' conn l (orphan pp))
                                      -- adopt par p'
hear' :: Connected -> Label -> Proc -> Maybe Proc
hear' conn (ENTER a)     (Excl b True)      | a==b               = Just $ Excl b False
hear' conn (EXIT a)      (Excl b False)     | a==b               = Just $ Excl b True
hear' conn (IRVR a _)    (Irv b v)          | a==b               = Nothing -- Just $ Irv b v
hear' conn (IRVW a v)    (Irv b _)          | a==b               = Just $ Irv b v
hear' conn (RCV a _)     (QElem b n (v:vs)) | a==b               = Just $ QElem b n vs
hear' conn (RCV a _)     (QElem b n [])     | a==b               = Nothing -- Just $ QElem b n []
hear' conn (SND a v _)   (QElem b n vs) 
        | a `conn` b && length vs < n                            = Just $ QElem b n (vs++[v])
        | a `conn` b                                             = Nothing -- Just $ QElem b n vs
hear' conn (SND a _ _)   (Run b t _ n s)    | trig conn a s      = Just $ Run b t Pending n s
hear' conn (RD a _)      (DElem b _ v)      | a==b               = Just $ DElem b False v
hear' conn (WR a v)      (DElem b _ _)      | a `conn` b         = Just $ DElem b True (Ok v)
hear' conn (WR a _)      (Run b t _ n s)    | trig conn a s      = Just $ Run b t Pending n s
hear' conn (UP a _)      (DElem b u v)      | a==b               = Nothing -- Just $ DElem b u v
hear' conn (INV a)       (DElem b _ _)      | a `conn` b         = Just $ DElem b True NO_DATA
hear' conn (CALL a v _)  (Run b t (Serving cs vs) n s)
        | trig conn a s && a `notElem` cs                        = Just $ Run b t (Serving (cs++[a]) (vs++[v])) n s
        | trig conn a s                                          = Just $ Run b t (Serving cs vs) n s 
  -- ^ Note: silently ignoring the value |v| from the |CALL| if |a| is already being served
hear' conn (RES a _)     (Op b (v:vs))      | a==b               = Just $ Op b vs
hear' conn (RES a _)     (Op b [])          | a==b               = Nothing -- Just $ Op b []
hear' conn (RET a v)     (Op b vs)          | a==b               = Just $ Op b (vs++[v])
hear' conn (TERM a)      (Run b t act n s)  | a==b               = Just $ Run b t act (n-1) s
hear' conn (TICK a)      (Run b t _ n s)    | a==b               = Just $ Run b t Pending n s
hear' conn (DELTA d)     (Run b 0.0 act n s)                     = Nothing -- Just $ Run b 0.0 act n s
hear' conn (DELTA d)     (Run b t act n s)                       = Just $ Run b (t-d) act n s
hear' conn (DELTA d)     (Timer b t t0)                          = Just $ Timer b (t-d) t0
hear' conn (DELTA d)     (Src b ((t,v):vs))                      = Just $ Src b ((t-d,v):vs)
hear' conn (WR a v)      (Sink b t vs)      | a `conn` b         = Just $ Sink b 0.0 ((t,v):vs)
hear' conn (DELTA d)     (Sink b t vs)                           = Just $ Sink b (t+d) vs
hear' conn label         proc                                    = Nothing

initSources state                                 = state { procs = foldl initialize processes inits }
  where inits                                     = [ WR a v | Src a ((0.0,v):_) <- processes ]
        conn                                      = connected (conns state)
        processes                                 = procs state
        init (WR a v) (Src b (_:vs)) | a==b       = Src b vs
        init (WR a v) (DElem b _ _)  | a `conn` b = DElem b False (Ok v)
        init label    proc                        = proc
        initialize processes label                = map (init label) processes
