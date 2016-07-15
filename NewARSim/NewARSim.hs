{-
Copyright (c) 2014-2016, Johan Nordlander, Jonas Duregård, Michał Pałka,
                         Patrik Jansson and Josef Svenningsson
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
   * Neither the name of the Chalmers University of Technology nor the names of its
     contributors may be used to endorse or promote products derived from this
     software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module NewARSim
  ( module NewARSim
  , Typeable
  , Data
  , mkStdGen
  , StdGen
  ) where

import           Control.Monad.Catch
import           Control.Monad.Operational
import           Control.Monad.Identity     hiding (void)
import           Control.Monad.State.Lazy   hiding (void)
import           Data.Char                         (ord, chr)
import           Data.List
import           Data.Map                          (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Set                          (Set)
import qualified Data.Set                        as Set
import           Data.Maybe
import qualified Data.Vector.Storable            as SV
import           Data.Vector.Storable              ((!), (//))
import qualified Data.Vector.Storable.Mutable    as MSV
import           Data.Tuple                         (swap)
import           Dynamics
import           Foreign.C
import           Foreign.Marshal            hiding (void)
import           Foreign.Ptr
import           Foreign.Storable
import           System.Environment
import           System.Exit
import           System.FilePath (FilePath)
import           System.Directory (removeFile)
import           System.IO
import           System.IO.Error
import           System.IO.Unsafe
import           System.Random
import           System.Posix hiding (getEnvironment)
import           System.Process (ProcessHandle, createProcess, env, proc, terminateProcess)
import qualified Unsafe.Coerce
import           Test.QuickCheck hiding (collect)
import           Test.QuickCheck.Property (unProperty)
import qualified Test.QuickCheck.Property as QCP
import qualified Test.QuickCheck.Text as QCT
import qualified Test.QuickCheck.Exception as QCE
import qualified Test.QuickCheck.State as QCS

import Debug.Trace

-- The RTE monad -------------------------------------------------------------

type RTE c a                = Program (RTEop c) a

data RTEop c a where
    Enter                   :: ExclusiveArea c -> RTEop c (StdRet ())
    Exit                    :: ExclusiveArea c -> RTEop c (StdRet ())
    IrvWrite                :: Data a => InterRunnableVariable a c -> a -> RTEop c (StdRet ())
    IrvRead                 :: Data a => InterRunnableVariable a c -> RTEop c (StdRet a)
    Send                    :: Data a => DataElement Queued a Provided c -> a -> RTEop c (StdRet ())
    Receive                 :: Data a => DataElement Queued a Required c -> RTEop c (StdRet a)
    Write                   :: Data a => DataElement Unqueued a Provided c -> a -> RTEop c (StdRet ())
    Read                    :: Data a => DataElement Unqueued a Required c -> RTEop c (StdRet a)
    IsUpdated               :: DataElement Unqueued a Required c -> RTEop c (StdRet Bool)
    Invalidate              :: DataElement Unqueued a Provided c -> RTEop c (StdRet ())
    Call                    :: Data a => ClientServerOperation a b Required c -> a -> RTEop c (StdRet ())
    Result                  :: Data b => ClientServerOperation a b Required c -> RTEop c (StdRet b)
    Printlog                :: Data a => ProbeID -> a -> RTEop c ()

rteEnter                   :: ExclusiveArea c -> RTE c (StdRet ())
rteExit                    :: ExclusiveArea c -> RTE c (StdRet ())
rteIrvWrite                :: Data a => InterRunnableVariable a c -> a -> RTE c (StdRet ())
rteIrvRead                 :: Data a => InterRunnableVariable a c -> RTE c (StdRet a)
rteSend                    :: Data a => DataElement Queued a Provided c -> a -> RTE c (StdRet ())
rteReceive                 :: Data a => DataElement Queued a Required c -> RTE c (StdRet a)
rteWrite                   :: Data a => DataElement Unqueued a Provided c -> a -> RTE c (StdRet ())
rteRead                    :: Data a => DataElement Unqueued a Required c -> RTE c (StdRet a)
rteIsUpdated               :: DataElement Unqueued a Required c -> RTE c (StdRet Bool)
rteInvalidate              :: DataElement Unqueued a Provided c -> RTE c (StdRet ())
rteCall                    :: (Data a, Data b) => ClientServerOperation a b Required c -> a -> RTE c (StdRet b)
rteCallAsync               :: Data a => ClientServerOperation a b Required c -> a -> RTE c (StdRet ())
rteResult                  :: Data b => ClientServerOperation a b Required c -> RTE c (StdRet b)

printlog                    :: Data a => ProbeID -> a -> RTE c ()

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

printlog id val             = singleton $ Printlog id val

data StdRet a               = Ok a
                            | Error Int
                            | NO_DATA
                            | NEVER_RECEIVED
                            | LIMIT
                            | UNCONNECTED
                            | TIMEOUT
                            | IN_EXCLUSIVE_AREA
                            deriving Show

newtype DataElement q a r c             = DE Address      -- Async channel of "a" data
type    DataElem q a r                  = DataElement q a r Closed

newtype ClientServerOperation a b r c   = OP Address      -- Sync channel of an "a->b" service
type    ClientServerOp a b r            = ClientServerOperation a b r Closed

data Queued         -- Parameter q above
data Unqueued

data Required       -- Parameter r above
data Provided

data InitValue a                = InitValue a
data QueueLength a              = QueueLength a

newtype InterRunnableVariable a c   = IV Address
newtype ExclusiveArea c             = EX Address

type Time                   = Double

data Event c                = forall q a. DataReceivedEvent (DataElement q a Required c)
                            | TimingEvent Time
                            | InitEvent

data ServerEvent a b c      = OperationInvokedEvent (ClientServerOperation a b Provided c)

data Invocation             = Concurrent
                            | MinInterval Time
                            deriving (Eq)


-- * Simulator state
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data SimState = SimState 
  { procs       :: [Proc]
  , conns       :: [Conn]
  , simProbes   :: [Probe]
  , initvals    :: Map Address Value
  , nextA       :: Address
  }

instance Show SimState where
  show (SimState procs conns simProves initvals nextA) =
    unwords ["SimState", show procs, show conns, show initvals, show nextA]

-- * Processes
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              
data Proc 
  -- The second Int is equal to the number of instances spawned so far
  = forall c. Run       Address Time Act Int Int (Static c)
  -- The Int identifies the particular instance spawned by a runnable
  | forall c. RInst     Address Int (Maybe Client) [Address] (RTE c Value)
  |           Excl      Address Exclusive
  |           Irv       Address Value
  -- The Int separates the timer from its runnable
  |           Timer     Address Time Time Int
  |           QElem     Address Int [Value]
  |           DElem     Address Bool (StdRet Value)
  |           Op        Address [Value]
  |           Input     Address Value
  |           Output    Address Value

instance Show Proc where
  show (Run a t act n m _) = unwords ["Run", show a, show t, show act, show n, show m]
  show (RInst a n mc ax _) = unwords ["RInst", show (a, n), show mc, show ax]
  show (Excl a e)          = unwords ["Excl", show a, show e]
  show (Irv a _)           = unwords ["Irv", show a]
  show (Timer a _ _ n)     = unwords ["Timer", show a, show n]
  show (QElem a _ _)       = unwords ["QElem", show a]
  show (DElem a _ _)       = unwords ["DElem", show a]
  show (Op a _)            = unwords ["Op", show a]
  show (Input a _)         = unwords ["Input", show a]
  show (Output a _)        = unwords ["Output", show a]

-- The motivation for yet another type for external addresses are that these
-- carry the address of their target process, and will thus overwrite those
-- otherwise, in the same way that timers and runnable instances will do to
-- each other (and runnables).
data ProcAddress 
  = UniqueAddr Address 
  | RInstAddr  Address Int 
  | TimerAddr  Address Int
  | ExtAddr    Address
  deriving (Eq, Ord)

instance Show ProcAddress where
  -- Just assuming this is how these will be used:
  show pa = case pa of 
    UniqueAddr a  -> "RUN "      ++ show a
    RInstAddr a n -> "RINST "    ++ show a ++ " " ++ show n
    TimerAddr a n -> "TIMER "    ++ show a ++ " " ++ show n
    ExtAddr a     -> "EXTERNAL " ++ show a

-- | Get the address of the process. If it's a runnable instance, also get it's 
-- unique id.
procAddress :: Proc -> ProcAddress 
procAddress (Run   a _ _ _ _ _) = UniqueAddr a
procAddress (RInst a n _ _ _)   = RInstAddr  a n
procAddress (Timer  a _ _ n)    = TimerAddr  a n 
procAddress (Excl  a _)         = UniqueAddr a
procAddress (Irv  a _)          = UniqueAddr a
procAddress (QElem  a _ _)      = UniqueAddr a
procAddress (DElem  a _ _)      = UniqueAddr a
procAddress (Op a _)            = UniqueAddr a
procAddress (Input a _)         = ExtAddr a
procAddress (Output a _)        = ExtAddr a

-- * Connection relations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type Conn = (Address, Address)
type ConnRel = Address -> Address -> Bool

rev :: ConnRel -> ConnRel
rev conn a b                = b `conn` a

type ProbeID                = String
type Probe                  = (ProbeID, Label -> Maybe Value)
probeID :: Probe -> ProbeID
probeID = fst
runProbe :: Probe -> Label -> Maybe Value
runProbe = snd


type Address = Int
type Client = Address

data Act
  = Idle
  | Pending
  | Serving [Client] [Value]
  deriving (Show)

data Exclusive = Free | Taken
  deriving (Show)

data Static c = Static 
  { triggers       :: [Address]
  , invocation     :: Invocation
  , implementation :: Value -> RTE c Value
  }

state0 :: SimState
state0 = SimState 
  { procs     = [] 
  , conns     = []
  , simProbes = []
  , initvals  = Map.empty
  , nextA     = 0
  }

apInit :: [Conn] -> Map Address Value -> Proc -> Proc
apInit conn mp p@(DElem a f NO_DATA) = 
  case [ v | (b,a') <- conn, a'==a, Just v <- [Map.lookup b mp] ] of
    [v] -> DElem a f (Ok v)
    _   -> p
apInit conn mp p = p

-- The AR monad ---------------------------------------------------------------

data ARInstr c a where
    NewAddress              :: ARInstr c Address
    NewProcess              :: Proc -> ARInstr c ()
    ModProcess              :: (Proc -> Proc) -> ARInstr c ()
    NewProbe                :: String -> (Label -> Maybe Value) -> ARInstr c ()
    NewInit                 :: Address -> Value -> ARInstr c ()
    NewComponent            :: AR d a -> ARInstr c a  -- Too strong requirement on the argument.
    NewConnection           :: Conn -> ARInstr c ()

type AR c a                 = Program (ARInstr c) a

data Open c
data Closed

type Atomic c a             = AR (Open c) a
type AUTOSAR a              = AR Closed a

runAR                       :: AR c a -> SimState -> (a,SimState)
runAR sys st                = run sys st
  where
    run                     :: AR c a -> SimState -> (a,SimState)
    run sys st              = run' (view sys) st
    run'                    :: ProgramView (ARInstr c) a -> SimState -> (a,SimState)
    run' (NewAddress :>>= sys) st
                            = run (sys (nextA st)) (st { nextA = nextA st + 1 })
    run' (NewProcess p :>>= sys) st
                            = run (sys ()) (st { procs = p : procs st })
    run' (ModProcess f :>>= sys) st
                            = run (sys ()) (st { procs = map f (procs st) })
    run' (NewProbe s f :>>= sys) st
                            = run (sys ()) (st { simProbes = (s,f) : simProbes st })
    run' (NewInit a v :>>= sys) st
                            = run (sys ()) (st { initvals = Map.insert a v (initvals st) })
    run' (NewComponent subsys :>>= sys) st
                            = let (a,st') = runAR subsys st in run (sys a) st'
    run' (NewConnection conn :>>= sys) st
                            = run (sys ()) (st { conns = addTransitive conn (conns st) })
    run' (Return a) st      = (a,st)

addTransitive (a,b) conns   = (a,b) : [ (a,c) | (x,c) <- conns, b==x ] ++ [ (c,b) | (c,x) <- conns, a==x ] ++ conns

initialize                  :: AUTOSAR a -> (a,SimState)
initialize sys              = (a, st { procs = map (apInit (conns st) (initvals st)) (procs st) })
  where (a,st)              = runAR sys state0

-- Restricting connections ----------------------------------------------------



class Port p where
    providedPort     :: Atomic c (p Provided c)
    requiredPort     :: Atomic c (p Required c)
    connect          :: p Provided Closed -> p Required Closed -> AUTOSAR ()
    providedDelegate :: [p Provided Closed] -> AUTOSAR (p Provided Closed)
    requiredDelegate :: [p Required Closed] -> AUTOSAR (p Required Closed)

class ComSpec p where
    type ComSpecFor p :: *
    comSpec :: p c -> ComSpecFor p -> Atomic c ()


instance Data a => ComSpec (DataElement Unqueued a Provided) where
    type ComSpecFor (DataElement Unqueued a Provided) = InitValue a
    comSpec (DE a) (InitValue x) = do
        newInit a (toValue x)

instance Data a => ComSpec (DataElement Unqueued a Required) where
    type ComSpecFor (DataElement Unqueued a Required) = InitValue a
    comSpec (DE a) (InitValue x) = do
        modProcess f
      where
        f (DElem b s _) | a==b  = DElem b s (Ok (toValue x))
        f p                     = p


instance Data a => Port (DataElement Unqueued a) where
    providedPort = do
        a <- newAddress
        return (DE a)
    requiredPort = do
        a <- newAddress
        newProcess (DElem a False NO_DATA)
        return (DE a)
    connect (DE a) (DE b) = newConnection (a,b)
    providedDelegate ps = do
        a <- newAddress
        mapM newConnection [ (p,a) | DE p <- ps ]
        return (DE a)
    requiredDelegate ps = do
        a <- newAddress
        mapM newConnection [ (a,p) | DE p <- ps ]
        return (DE a)

instance Data a => ComSpec (DataElement Queued a Required) where
    type ComSpecFor (DataElement Queued a Required) = QueueLength Int
    comSpec (DE a) (QueueLength l) = do
        modProcess f
      where
        f (QElem b _ vs) | a==b = QElem b l vs
        f p                     = p

instance Port (DataElement Queued a) where
    providedPort = do a <- newAddress; return (DE a)
    requiredPort = do a <- newAddress; newProcess (QElem a 10 []); return (DE a)
    connect (DE a) (DE b) = newConnection (a,b)
    providedDelegate ps = do
        a <- newAddress
        mapM newConnection [ (p,a) | DE p <- ps ]
        return (DE a)
    requiredDelegate ps = do
        a <- newAddress
        mapM newConnection [ (a,p) | DE p <- ps ]
        return (DE a)

instance ComSpec (ClientServerOperation a b Provided) where
    type ComSpecFor (ClientServerOperation a b Provided) = QueueLength Int
    comSpec (OP a) (QueueLength l) = do
        -- There is a queueLength defined in AUTOSAR, but it is unclear what is means
        -- for a ClientServerOperation: argument or result buffer length? Or both?
        return ()

instance Port (ClientServerOperation a b) where
    providedPort = do a <- newAddress; return (OP a)
    requiredPort = do a <- newAddress;
                      newProcess (Op a []);
                      return (OP a)
    connect (OP a) (OP b) = newConnection (a,b)
    providedDelegate ps = do
        a <- newAddress
        mapM newConnection [ (p,a) | OP p <- ps ]
        return (OP a)
    requiredDelegate ps = do
        a <- newAddress
        mapM newConnection [ (a,p) | OP p <- ps ]
        return (OP a)


connectEach :: Port p => [p Provided Closed] -> [p Required Closed] -> AUTOSAR ()
connectEach prov req = forM_ (prov `zip` req) $ uncurry connect 


class Addressed a where
    type Payload a              :: *
    address                     :: a -> Address

instance Addressed (InterRunnableVariable a c) where
    type Payload (InterRunnableVariable a c) = a
    address (IV n)              = n

instance Addressed (DataElement q a r c) where
    type Payload (DataElement q a r c) = a
    address (DE n)              = n

instance Addressed (ClientServerOperation a b r c) where
    type Payload (ClientServerOperation a b r c) = a
    address (OP n)              = n


type family Seal a where
    Seal (k Required c)             = k Required Closed
    Seal (k Provided c)             = k Provided Closed
    Seal (k a b c d e f g h)        = k (Seal a) (Seal b) (Seal c) (Seal d) (Seal e) (Seal f) (Seal g) (Seal h)
    Seal (k a b c d e f g)          = k (Seal a) (Seal b) (Seal c) (Seal d) (Seal e) (Seal f) (Seal g)
    Seal (k a b c d e f)            = k (Seal a) (Seal b) (Seal c) (Seal d) (Seal e) (Seal f)
    Seal (k a b c d e)              = k (Seal a) (Seal b) (Seal c) (Seal d) (Seal e)
    Seal (k a b c d)                = k (Seal a) (Seal b) (Seal c) (Seal d)
    Seal (k a b c)                  = k (Seal a) (Seal b) (Seal c)
    Seal (k a b)                    = k (Seal a) (Seal b)
    Seal (k a)                      = k (Seal a)
    Seal k                          = k


seal                                :: a -> Seal a
seal                                = Unsafe.Coerce.unsafeCoerce

type family Unseal a where
    Unseal (a->b)                   = Seal a -> Unseal b
    Unseal a                        = a

class Sealer a where
    sealBy                          :: Unseal a -> a
    sealBy                          = undefined

instance Sealer b => Sealer (a -> b) where
    sealBy f a                      = sealBy (f (seal a))

instance {-# OVERLAPPABLE #-} (Unseal a ~ a) => Sealer a where
    sealBy                          = id




-- Derived AR operations ------------------------------------------------------

interRunnableVariable       :: Data a => a -> Atomic c (InterRunnableVariable a c)
exclusiveArea               :: Atomic c (ExclusiveArea c)
runnable                    :: Invocation -> [Event c] -> RTE c a -> Atomic c ()
serverRunnable              :: (Data a, Data b) =>
                                Invocation -> [ServerEvent a b c] -> (a -> RTE c b) -> Atomic c ()
composition                 :: AUTOSAR a -> AUTOSAR a
atomic                      :: (forall c. Atomic c a) -> AUTOSAR a

composition c               = singleton $ NewComponent c
atomic c                    = singleton $ NewComponent c

newConnection c             = singleton $ NewConnection c
newAddress                  = singleton   NewAddress
newProcess p                = singleton $ NewProcess p
modProcess f                = singleton $ ModProcess f
newInit a v                 = singleton $ NewInit a v

interRunnableVariable val   = do a <- newAddress; newProcess (Irv a (toValue val)); return (IV a)
exclusiveArea               = do a <- newAddress; newProcess (Excl a Free); return (EX a)

-- Added an integer identifier to the Timer constructor or we cannot differ it 
-- from its Run master by means of address
runnable inv events code = 
  do a <- newAddress
     mapM_ (\(t, n) -> newProcess (Timer a 0.0 t n)) (periods `zip` [0..])
     newProcess (Run a 0.0 act 0 0 (Static watch inv code'))
  where periods             = [ t | TimingEvent t <- events ]
        watch               = [ a | DataReceivedEvent (DE a) <- events ]
        act                 = if null [ () | InitEvent <- events ] then Idle else Pending
        code'               = \dyn -> code >> return dyn

serverRunnable inv ops code = do a <- newAddress
                                 newProcess (Run a 0.0 act 0 0 (Static watch inv code'))
  where watch               = [ a | OperationInvokedEvent (OP a) <- ops ]
        act                 = Serving [] []
        code'               = fmap toValue . code . fromDyn

fromDyn                     :: Data a => Value -> a
fromDyn                     = value'

-- TODO: add Reading/Writing classes instead of Addressed?
probeRead                   :: (Addressed t, Data (Payload t)) => String -> t -> AR c ()
probeRead s x              = singleton $ NewProbe s g
  where
    g (RD b (Ok v))    | a==b    = Just v
    g (RCV b (Ok v))   | a==b    = Just v
    g (IRVR b (Ok v))  | a==b    = Just v
    g (RES b (Ok v))   | a==b    = Just v
    g _                         = Nothing
    a = address x


probeWrite                  :: (Addressed t, Data (Payload t)) => String -> t -> AR c ()
probeWrite s x            = singleton $ NewProbe s g
  where
    g (IRVW b v)     | a==b     = Just v
    g (WR b v)       | a==b     = Just v
    g (SND b v _)    | a==b     = Just v -- Not sure about these.
    g (CALL b v _)   | a==b     = Just v
    g (RET b v)      | a==b     = Just v
    g _                     = Nothing
    a = address x
{-
probeWrite'                 :: (Data b, Data a, Addressed (e a r c)) => String -> e a r c -> AR c' ()
probeWrite' s x f    = singleton $ NewProbe s g
  where
    g (WR b v) | a==b       = Just (toValue $ f $ value' v) -- TODO: Do we know this is always of type a?


    g _                     = Nothing
    a = address x
-}



data Label                  = ENTER Address
                            | EXIT  Address
                            | IRVR  Address (StdRet Value)
                            | IRVW  Address Value
                            | RCV   Address (StdRet Value)
                            | SND   Address Value (StdRet ())
                            | RD    Address (StdRet Value)
                            | WR    Address Value
                            | UP    Address (StdRet Value)
                            | INV   Address
                            | CALL  Address Value (StdRet ())
                            | RES   Address (StdRet Value)
                            | RET   Address Value
                            | NEW   Address Int
                            | TERM  Address
                            | TICK  Address
                            | DELTA Time
                            | VETO
                            deriving Show

labelText :: Label -> String
labelText l = case l of
          ENTER a            -> "ENTER:"++show a
          EXIT  a            -> "EXIT:" ++show a
          IRVR  a     ret    -> "IRVR:" ++show a
          IRVW  a val        -> "IRVW:" ++show a++":"++show val
          RCV   a     ret    -> "RCV:"  ++show a
          SND   a val ret    -> "SND:"  ++show a++":"++show val
          RD    a     ret    -> "RD:"   ++show a
          WR    a val        -> "WR:"   ++show a++":"++show val
          UP    a     ret    -> "UP:"   ++show a
          INV   a            -> "INV:"  ++show a
          CALL  a val ret    -> "CALL:" ++show a++":"++show val
          RES   a     ret    -> "RES:"  ++show a
          RET   a val        -> "RET:"  ++show a++":"++show val
          NEW   a _          -> "NEW:"  ++show a
          TERM  a            -> "TERM:" ++show a
          TICK  a            -> "TICK:" ++show a
          DELTA t            -> "DELTA:"++show t
          VETO               -> "VETO"

labelAddress :: Label -> Maybe Address
labelAddress l = case l of
          ENTER a            -> Just a
          EXIT  a            -> Just a
          IRVR  a     ret    -> Just a
          IRVW  a val        -> Just a
          RCV   a     ret    -> Just a
          SND   a val ret    -> Just a
          RD    a     ret    -> Just a
          WR    a val        -> Just a
          UP    a     ret    -> Just a
          INV   a            -> Just a
          CALL  a val ret    -> Just a
          RES   a     ret    -> Just a
          RET   a val        -> Just a
          NEW   a _          -> Just a
          TERM  a            -> Just a
          TICK  a            -> Just a
          DELTA t            -> Nothing
          VETO               -> Nothing

maySay :: Proc -> Label
maySay (Run a 0.0 Pending n m s)
    | n == 0 || invocation s == Concurrent     = NEW   a m
maySay (Run a 0.0 (Serving (c:cs) (v:vs)) n m s)
    | n == 0 || invocation s == Concurrent     = NEW   a m
maySay (Run a t act n m s)  | t > 0.0          = DELTA t
maySay (Timer a 0.0 t _)                       = TICK  a
maySay (Timer a t t0 _)   | t > 0.0            = DELTA t
maySay (RInst a _ c ex code)                   = maySay' (view code)
  where maySay' (Enter (EX x)      :>>= cont)  = ENTER x
        maySay' (Exit  (EX x)      :>>= cont)  = case ex of
                                                     y:ys | y==x -> EXIT x
                                                     _           -> VETO
        maySay' (IrvRead  (IV s)   :>>= cont)  = IRVR  s NO_DATA
        maySay' (IrvWrite (IV s) v :>>= cont)  = IRVW  s (toValue v)
        maySay' (Receive (DE e)    :>>= cont)  = RCV   e NO_DATA
        maySay' (Send    (DE e) v  :>>= cont)  = SND   e (toValue v) void
        maySay' (Read    (DE e)    :>>= cont)  = RD    e NO_DATA
        maySay' (Write   (DE e) v  :>>= cont)  = WR    e (toValue v)
        maySay' (IsUpdated  (DE e) :>>= cont)  = UP    e NO_DATA
        maySay' (Invalidate (DE e) :>>= cont)  = INV   e
        maySay' (Call   (OP o) v   :>>= cont)  = CALL  o (toValue v) NO_DATA
        maySay' (Result (OP o)     :>>= cont)  = RES   o NO_DATA
        maySay' (Return v)                     = case c of
                                                     Just b  -> RET  b v
                                                     Nothing -> TERM a
        maySay' (Printlog i v      :>>= cont)  = maySay' (view (cont ()))
maySay (Input a v)                             = WR a v
maySay _                                       = VETO


say :: Label -> Proc -> [Update Proc]
say (NEW _ _) (Run a _ Pending n m s)                   = [ Update $ Run a (minstart s) Idle (n + 1) (m + 1) s           
                                                          , Update $ RInst a m Nothing [] (implementation s (toValue ())) ]
say (NEW _ _) (Run a _ (Serving (c:cs) (v:vs)) n m s)   = [ Update $ Run a (minstart s) (Serving cs vs) (n + 1) (m + 1) s
                                                          , Update $ RInst a m (Just c) [] (implementation s v) ]
say (DELTA d) (Run a t act n m s)                       = [ Update $ Run a (t - d) act n m s]
say (TICK _)  (Timer a _ t n)                           = [ Update $ Timer a t t n]
say (DELTA d) (Timer a t t0 n)                          = [ Update $ Timer a (t - d) t0 n]
say label     (RInst a n c ex code)                     = say' label (view code)
  where say' (ENTER _)      (Enter (EX x) :>>= cont)    = [ Update $ RInst a n c (x:ex)   (cont void)]
        say' (EXIT _)       (Exit (EX x)  :>>= cont)    = [ Update $ RInst a n c ex       (cont void)]
        say' (IRVR _ res)   (IrvRead _    :>>= cont)    = [ Update $ RInst a n c ex       (cont (fromStdDyn res))]
        say' (IRVW _ _)     (IrvWrite _ _ :>>= cont)    = [ Update $ RInst a n c ex       (cont void)]
        say' (RCV _ res)    (Receive _    :>>= cont)    = [ Update $ RInst a n c ex       (cont (fromStdDyn res))]
        say' (SND _ _ res)  (Send _ _     :>>= cont)    = [ Update $ RInst a n c ex       (cont res)]
        say' (RD _ res)     (Read _       :>>= cont)    = [ Update $ RInst a n c ex       (cont (fromStdDyn res))]
        say' (WR _ _)       (Write _ _    :>>= cont)    = [ Update $ RInst a n c ex       (cont void)]
        say' (UP _ res)     (IsUpdated _  :>>= cont)    = [ Update $ RInst a n c ex       (cont (fromStdDyn res))]
        say' (INV _)        (Invalidate _ :>>= cont)    = [ Update $ RInst a n c ex       (cont void)]
        say' (CALL _ _ res) (Call _ _     :>>= cont)    = [ Update $ RInst a n c ex       (cont res)]
        say' (RES _    res) (Result _     :>>= cont)    = [ Update $ RInst a n c ex       (cont (fromStdDyn res))]
        say' (RET _ _)      (Return v)                  = [ Update $ RInst a n Nothing ex (return (toValue ()))]
        say' (TERM _)       (Return _)                  = [ Remove $ RInst a n c ex code ] -- Can carry any payload so long as address and index is right
        say' label          (Printlog i v :>>= cont)    = say' label (view (cont ()))
say (WR _ _)  (Input a v)                               = [ Remove $ Input a v ] -- Can carry any payload

mayLog (RInst a n c ex code)                            = mayLog' (view code)
  where mayLog' :: ProgramView (RTEop c) a -> Logs
        mayLog' (Printlog i v :>>= cont)                = (i,toValue v) : mayLog' (view (cont ()))
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
mayHear conn (ENTER a)      (Excl b Taken)    | a==b           = VETO
mayHear conn (EXIT a)       (Excl b Free)     | a==b           = VETO
mayHear conn (IRVR a _)     (Irv b v)         | a==b           = IRVR a (Ok v)
mayHear conn (IRVW a v)     (Irv b _)         | a==b           = IRVW a v
mayHear conn (RCV a _)      (QElem b n (v:_)) | a==b           = RCV a (Ok v)
mayHear conn (RCV a _)      (QElem b n [])    | a==b           = RCV a NO_DATA
mayHear conn (SND a v res)  (QElem b n vs)
       | a `conn` b && length vs < n                           = SND a v res
       | a `conn` b                                            = SND a v LIMIT
mayHear conn (RD a _)       (DElem b u v)     | a==b           = RD a v
mayHear conn (UP a _)       (DElem b u _)     | a==b           = UP a (Ok (toValue u))
mayHear conn (CALL a v res) (Run b t (Serving cs vs) n m s)
       | trig (rev conn) a s  &&  a `notElem` cs               = CALL a v void
       | trig (rev conn) a s                                   = CALL a v LIMIT
mayHear conn (RES a _)      (Op b (v:vs))     | a==b           = RES a (Ok v)
mayHear conn (RES a _)      (Op b [])         | a==b           = VETO  -- RES a NO_DATA
mayHear conn (DELTA d)      (Run _ t _ _ _ _) | d > t && t > 0 = VETO
mayHear conn (DELTA d)      (Timer _ t _ _)   | d > t          = VETO
mayHear conn label          _                                  = label

hear :: ConnRel -> Label -> Proc -> Update Proc
hear conn (ENTER a)     (Excl b Free)      | a==b          = Update $ Excl b Taken
hear conn (EXIT a)      (Excl b Taken)     | a==b          = Update $ Excl b Free
hear conn (IRVR a _)    (Irv b v)          | a==b          = Unchanged
hear conn (IRVW a v)    (Irv b _)          | a==b          = Update $ Irv b v
hear conn (RCV a _)     (QElem b n (v:vs)) | a==b          = Update $ QElem b n vs
hear conn (RCV a _)     (QElem b n [])     | a==b          = Unchanged 
hear conn (SND a v _)   (QElem b n vs)
        | a `conn` b && length vs < n                      = Update $ QElem b n (vs++[v])
        | a `conn` b                                       = Unchanged 
hear conn (SND a _ _)   (Run b t _ n m s)  | trig conn a s = Update $ Run b t Pending n m s
hear conn (RD a _)      (DElem b _ v)      | a==b          = Update $ DElem b False v
hear conn (WR a v)      (DElem b _ _)      | a `conn` b    = Update $ DElem b True (Ok v)
hear conn (WR a _)      (Run b t _ n m s)  | trig conn a s = Update $ Run b t Pending n m s
hear conn (UP a _)      (DElem b u v)      | a==b          = Unchanged 
hear conn (INV a)       (DElem b _ _)      | a `conn` b    = Update $ DElem b True NO_DATA
hear conn (CALL a v _)  (Run b t (Serving cs vs) n m s)
        | trig (rev conn) a s && a `notElem` cs            = Update $ Run b t (Serving (cs++[a]) (vs++[v])) n m s
        | trig (rev conn) a s                              = Unchanged 
hear conn (RES a _)     (Op b (v:vs))         | a==b       = Update $ Op b vs
hear conn (RES a _)     (Op b [])             | a==b       = Unchanged 
hear conn (RET a v)     (Op b vs)             | a==b       = Update $ Op b (vs++[v])
hear conn (TERM a)      (Run b t act n m s)   | a==b       = Update $ Run b t act (n-1) m s
hear conn (TICK a)      (Run b t _ n m s)     | a==b       = Update $ Run b t Pending n m s
hear conn (DELTA d)     (Run b 0.0 act n m s)              = Unchanged 
hear conn (DELTA d)     (Run b t act n m s)                = Update $ Run b (t-d) act n m s
hear conn (DELTA d)     (Timer b t t0 n)                   = Update $ Timer b (t-d) t0 n
hear conn (WR a v)      (Output b _)       | a `conn` b    = Update $ Output b v
hear conn label         proc                               = Unchanged 

-- * 'step' and 'explore'
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Replaces fold of @(mayHear conn)@.
respond :: ConnRel -> [Proc] -> Label -> Label
respond _    _      VETO      = VETO
respond _    _      (TERM a)  = TERM a
respond _    _      (TICK a)  = TICK a
respond _    _      (INV a)   = INV a
respond _    _      (WR a v)  = WR a v
respond _    _      (RET a v) = RET a v
respond _    []     label     = label
respond conn (p:ps) label     = respond conn ps acc
  where acc = mayHear conn label p

step :: ConnRel -> ProcMap -> [SchedulerOption]
step conn pm = explore conn pm [] (pmapElems pm)

explore :: ConnRel -> ProcMap -> [Proc] -> [Proc] -> [SchedulerOption]
explore _    _  _   []     = []
explore conn pm pre (p:ps) = 
  case response conn pm (pre, ps) p of 
    VETO  ->          explore conn pm (p:pre) ps 
    label -> commit : explore conn pm (p:pre) ps
      where
        broadcast      = say label p ++ hear1 conn label (pmapDelete p pm)
        commit         = (label, procAddress p, logs label, broadcast)
        logs (DELTA _) = []
        logs _         = mayLog p

-- | Let only those concerned hear the broadcast (to some extent).
hear1 :: ConnRel -> Label -> ProcMap -> [Update Proc]
hear1 conn label pm = 
  case label of
    -- On target
    ENTER a   -> [hear conn label (pmapLookup pm (UniqueAddr a))]
    EXIT a    -> [hear conn label (pmapLookup pm (UniqueAddr a))]
    IRVR a _  -> [hear conn label (pmapLookup pm (UniqueAddr a))]
    IRVW a _  -> [hear conn label (pmapLookup pm (UniqueAddr a))]
    RCV a _   -> [hear conn label (pmapLookup pm (UniqueAddr a))]
    RD a _    -> [hear conn label (pmapLookup pm (UniqueAddr a))]
    UP a _    -> [hear conn label (pmapLookup pm (UniqueAddr a))]
    RES a _   -> [hear conn label (pmapLookup pm (UniqueAddr a))]
    RET a _   -> [hear conn label (pmapLookup pm (UniqueAddr a))]
    TERM a    -> [hear conn label (pmapLookup pm (UniqueAddr a))]
    TICK a    -> [hear conn label (pmapLookup pm (UniqueAddr a))]

    -- Not on target
    CALL {}   -> map (hear conn label) (pmapElems pm)
    INV {}    -> map (hear conn label) (pmapElems pm)
    SND a _ _ -> map (hear conn label) (pmapElems pm)
    WR a _    -> map (hear conn label) (pmapElems pm)
    DELTA {}  -> map (hear conn label) (pmapElems pm)
    _         -> [Unchanged]

-- | Agree on labels before broadcast.
response :: ConnRel -> ProcMap -> ([Proc], [Proc]) -> Proc -> Label
response conn pm (as, bs) p = 
  case maySay p of 
    label -> case label of
      -- On target
      ENTER a  -> mayHear conn label (pmapLookup pm (UniqueAddr a))
      EXIT a   -> mayHear conn label (pmapLookup pm (UniqueAddr a))
      IRVR a _ -> mayHear conn label (pmapLookup pm (UniqueAddr a))
      IRVW a _ -> mayHear conn label (pmapLookup pm (UniqueAddr a))
      RCV a _  -> mayHear conn label (pmapLookup pm (UniqueAddr a))
      RD a _   -> mayHear conn label (pmapLookup pm (UniqueAddr a))
      UP a _   -> mayHear conn label (pmapLookup pm (UniqueAddr a))
      RES a _  -> mayHear conn label (pmapLookup pm (UniqueAddr a))

      -- Not on target
      CALL {}  -> response' label 
      SND {}   -> response' label
      DELTA {} -> response' label 
      _        -> label
  where
    response' = respond conn as . respond conn bs 

-- * Address-to-process
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Interface for quickly accessing processes by address.

-- | Quicker process lookup during simulation. Used in conjunction with 
-- 'response' and 'hear1'.
type ProcMap = Map ProcAddress Proc

-- | Generate process map from connections and processes.
pmapFromList :: [Proc] -> ProcMap
pmapFromList = Map.fromList . map (\p -> (procAddress p, p)) 

-- | Return all processes in the map.
pmapElems :: ProcMap -> [Proc]
pmapElems = Map.elems

-- | Lookup process by its address. 
pmapLookup :: ProcMap -> ProcAddress -> Proc
pmapLookup = (Map.!)

-- | Insert the (address, process) pair in the map.
pmapInsert :: Proc -> ProcMap -> ProcMap
pmapInsert p = Map.insert (procAddress p) p

-- | Delete the (address, process) pair from the map.
pmapDelete :: Proc -> ProcMap -> ProcMap
pmapDelete = Map.delete . procAddress  

-- | Bulk update of process map.
pmapUpdate :: ProcMap -> [Update Proc] -> ProcMap
pmapUpdate pm ps = foldr pmapInsert (foldr pmapDelete pm removals) updates
  where
    updates  = [p | Update p <- ps]
    removals = [p | Remove p <- ps]

-- Mark processes for update or removal.
data Update a
  = Update a
  | Remove a
  | Unchanged

-- * The simulator proper 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Logs = [(ProbeID, Value)]

type SchedulerOption = (Label, ProcAddress, Logs, [Update Proc])

-- Transitions no longer carry processes, since we do not re-use them. This
-- reduces memory usage.
data Transition = Trans 
  { transChoice :: Int
  , transLabel  :: Label
  , transActive :: ProcAddress 
  , transLogs   :: Logs
  } deriving Show

type Scheduler m = [SchedulerOption] -> m (Maybe (Transition, [Update Proc]))
type Trace = (SimState, [Transition])

traceLabels :: Trace -> [Label]
traceLabels = map transLabel . traceTrans

traceTrans :: Trace -> [Transition]
traceTrans = snd

traceProbes :: Trace -> [Probe]
traceProbes = simProbes . fst

traceLogs :: Trace -> Logs
traceLogs = concatMap transLogs . traceTrans


printRow :: Int -> Int -> (Int -> String) -> String
printRow width tot prt =
  intercalate "|" [ take width $ prt i ++ repeat ' ' | i <- [0..tot-1]]
printTraceRow :: (Label, Int) -> Int -> String
printTraceRow (lab, col) i 
  | col == i  = show lab
  | otherwise = ""
{-
traceTable :: Trace -> String
traceTable t = unlines $ prt (reverse cnames !!) : prt (const $ repeat '-') : (map (prt . printTraceRow) $ byRows f)
  where
  prt = printRow 10 lind
  (f, (lind, cnames)) = S.runState (mapM reallyAllocate $ toForest t) (0, [])
-}
traceTable :: Trace -> String
traceTable t@(_, tx) = unlines $
    prt ([show p | p <- Map.keys processes] !!): prt (const $ repeat '-') : [prt $ printTraceRow $ getRow tr | tr <- tx]
  where
  prt = printRow 17 (Map.size processes)
  processes = rankSet $ traceProcs t
  getRow :: Transition -> (Label, Int)
  getRow tr = (transLabel tr, processes Map.! transActive tr)

-- Return the set of addresses of all active processes
traceProcs :: Trace -> Set ProcAddress 
traceProcs (_, t) = foldl' (\acc trans -> Set.insert (transActive trans) acc) Set.empty t

rankSet :: Set a -> Map a Int
rankSet s = Map.fromDistinctAscList $ zip (Set.elems s) [0..]

-------------------------------------------------------------------------------
-- * Stand-alone simulation
-------------------------------------------------------------------------------

-- | Initialize the simulator with an initial state and run it.
simulation :: Monad m => Scheduler m -> AUTOSAR a -> m (a, Trace)
simulation sched sys = 
  do trs <- simulate sched conn procs1
     return (res, (state1, trs))
  where 
    procs1        = pmapFromList (procs state1)
    (res, state1) = initialize sys
    a `conn` b    = (a, b) `elem` conns state1 || a == b

-- Internal simulator function. Progresses simulation until there are no more
-- transitions to take.
-- simulate :: Monad m => Scheduler m -> ConnRel -> [Proc] -> m [Transition]
simulate :: Monad m => Scheduler m -> ConnRel -> ProcMap -> m [Transition]
simulate sched conn procs = 
  do next <- simulate1 sched conn procs
     case next of
       Nothing ->
         return []
       Just (trans, procs1) -> 
         (trans:) <$> simulate sched conn (pmapUpdate procs procs1)

-- Progresses simulation until there are no more transition alternatives.
simulate1 :: Monad m 
          => Scheduler m 
          -> ConnRel 
          -> ProcMap
          -> m (Maybe (Transition, [Update Proc]))
simulate1 sched conn procs
  | null alts               = return Nothing
  | otherwise               = maximumProgress sched alts
  where alts :: [SchedulerOption]
        alts                = step conn procs

-- Schedules work as long as work-steps are available. When no more work can be
-- done, @DELTA@-steps are scheduled.
maximumProgress :: Scheduler m -> Scheduler m
maximumProgress sched alts
  | null work               = sched deltas
  | otherwise               = sched work
  where (deltas,work)       = partition isDelta alts
        isDelta (DELTA _, _,_,_) = True
        isDelta _                = False


trivialSched :: Scheduler Identity
trivialSched alts = return $ Just (Trans 0 label active logs, procs)
  where 
    (label, active, logs, procs) = head alts

roundRobinSched :: Scheduler (State Int)
roundRobinSched alts = 
  do m <- get
     let n = (m+1) `mod` length alts
         (label, active, logs, procs) = alts !! n
     put n
     return $ Just (Trans n label active logs, procs)

randomSched :: Scheduler (State StdGen)
randomSched alts = 
  do n <- state next
     let (label, active, logs, procs) = alts !! (n `mod` length alts)
     return $ Just (Trans n label active logs, procs)

genSched :: Scheduler Gen
genSched alts = do
  ((label, active, logs, procs), n) <- elements $ zip alts [0..]
  return $ Just (Trans n label active logs, procs)

data SchedChoice            where
  TrivialSched        :: SchedChoice
  RoundRobinSched     :: SchedChoice
  RandomSched         :: StdGen -> SchedChoice
  -- This can be used to define all the other cases
  AnySched            :: Monad m => Scheduler m -> (forall a. m a -> a) -> SchedChoice

runSim                         :: SchedChoice -> AUTOSAR a -> (a,Trace)
runSim TrivialSched sys        = runIdentity (simulation trivialSched sys)
runSim RoundRobinSched sys     = evalState (simulation roundRobinSched sys) 0
runSim (RandomSched g) sys     = evalState (simulation randomSched sys) g
runSim (AnySched sch run) sys  = run (simulation sch sys)

execSim :: SchedChoice -> AUTOSAR a -> Trace
execSim sch sys = snd $ runSim sch sys

simulationRandG :: AUTOSAR a -> Gen (a, Trace)
simulationRandG a = simulation genSched a

{-
rerunSched :: Scheduler (State Trace)
rerunSched n ls = do
  (init,steps) <- get
  case steps of
    []         -> return Nothing -- Terminate
    (rr,(gpar,_)):rrs'  -> do
      put (init, rrs')
      case [x | x@(lab, (gparx,_)) <- ls, (lab `similarLabel` rr) && (gpar `siblingTo` gparx)] of
        []     -> do
           let rrs'' = shortCut n (snd gpar) rrs'
           put (init,rrs'')
           rerunSched n ls
        [x]  -> return $ Just $ x
        xs   -> return $ Just $
          case [x | x <- xs, orphan gpar `sameProcess` orphan (optionSpeaker x)] of
            []    -> head xs
            (x:_) -> x
-}

replaySched :: Scheduler (State Trace)
replaySched ls = do
  (init,steps) <- get
  case steps of
    []         -> return Nothing -- Terminate
    tr:rrs'  -> do
      put (init, rrs')
      let tlab  = transLabel tr
          taddr = transActive tr
          ls'   = zip ls [0..]
           -- First, try to match the label and the active process
      case [x | x@((lab, addr, _, _), _) <- ls',
                (lab `similarLabel` tlab) && (addr == taddr)] ++
           -- If that fails, try to match the label and similar active process
           [x | x@((lab, addr, _, _), _) <- ls',
                (lab `similarLabel` tlab) && (addr `siblingTo` taddr)] of
        []     -> replaySched ls -- If nothing matches, then just drop the event.
                                 -- Another option would be to save the event for later.
        ((lab, addr, logs, procs), n):xs -> return $ Just (Trans n lab addr logs, procs)

similarLabel :: Label -> Label -> Bool
similarLabel (IRVR n1 _)   (IRVR n2 _)   = n1 == n2
similarLabel (IRVW n1 _)   (IRVW n2 _)   = n1 == n2
similarLabel (RES n1 _)    (RES n2 _)    = n1 == n2
similarLabel (RET n1 _)    (RET n2 _)    = n1 == n2
similarLabel (RCV n1 _)    (RCV n2 _)    = n1 == n2
similarLabel (SND n1 _ _)  (SND n2 _ _)  = n1 == n2
-- Several more cases could be added.
similarLabel (ENTER n1)    (ENTER n2)    = n1 == n2
similarLabel (EXIT n1)     (EXIT n2)     = n1 == n2
similarLabel (RD n1 _)     (RD n2 _)     = n1 == n2
similarLabel (WR n1 _)     (WR n2 _)     = n1 == n2
similarLabel (UP n1 _)     (UP n2 _)     = n1 == n2
similarLabel (INV n1)      (INV n2)      = n1 == n2
similarLabel (CALL n1 _ _) (CALL n2 _ _) = n1 == n2
similarLabel (NEW n1 _)    (NEW n2 _)    = n1 == n2
similarLabel (TERM n1)     (TERM n2)     = n1 == n2
similarLabel (TICK n1)     (TICK n2)     = n1 == n2
similarLabel (DELTA _)     (DELTA _)     = True
similarLabel a             b             = False

sameNew (NEW n1 m1) (NEW n2 m2) = n1 == n2 && m1 == m2
sameNew _           _           = False

siblingTo :: ProcAddress -> ProcAddress -> Bool
siblingTo (UniqueAddr n1)  (UniqueAddr n2)  = n1 == n2
siblingTo (RInstAddr n1 _) (RInstAddr n2 _) = n1 == n2
siblingTo (TimerAddr n1 _) (TimerAddr n2 _) = n1 == n2
siblingTo _                _                = False

replaySimulation :: forall a. Trace -> AUTOSAR a -> (Trace, a)
replaySimulation tc m = swap $ evalState m' tc
    where m' :: State Trace (a, Trace)
          m' = simulation replaySched m

-- The original list represented by (pre, x, suf)
-- is pre ++ [x] ++ suf
type ListZipper a = ([a], a, [a])

shrinkTrace :: AUTOSAR a -> Trace -> [Trace]
shrinkTrace code tc@(init, tx) = map (\tx' -> fst $ replaySimulation(init, tx') code) $
  -- Remove a dynamic process and shift all later processes
  [ 
    [ if a == b && j > i then tr { transActive = RInstAddr b (j - 1) } else tr
      | tr <- tx, p'@(RInstAddr b j) <- [transActive tr]
      , p' /= p
      , not (transLabel tr `sameNew` NEW a i)] -- We also remove the spawn (but we should 
        -- also remove the event that triggered it)
  | p@(RInstAddr a i) <- procs ] ++
  -- Remove a process
  [ [ tr | tr <- tx, transActive tr /= p ] | p <- procs ] ++
  -- Remove arbitrary events
  [ tx' | tx' <- shrinkList (const []) tx ] ++
  -- Cluster events from the same process
  [ pre ++ [x, y] ++ suf1 ++ suf2
  | (pre, x, suf) <- allPositions tx
    -- Try to find an action y from the same process as action x in the suffix,
    -- and move it just after x.
  , (suf1@(_:_), y:suf2) <- [break (\tr -> transActive tr == transActive x) suf] ]
  where
  procs = Set.elems $ traceProcs tc
  -- removeCtxSwitch :: [Transition] -> [Transition] -> [[Transition]]
  allPositions :: [a] -> [ListZipper a]
  allPositions []     = []
  allPositions (x:xs) = [([], x, xs)] ++
    [(x:pre, y, suf) | (pre, y, suf) <- allPositions xs]

shrinkTrace' :: AUTOSAR a -> Trace -> [(ProcAddress, Trace)]
shrinkTrace' code tc@(init, tx) =
  [ (p, fst $ replaySimulation (init, [ tr | tr <- tx, transActive tr /= p ]) code) | p <- procs ]
  where
  procs = Set.elems $ traceProcs tc
  deleteLast pred l = reverse $ deleteBy (const pred) undefined $ reverse l

shrinkTrace'' :: AUTOSAR a -> Trace -> [Trace]
shrinkTrace'' code tc@(init, tx) =
  [ fst $ replaySimulation (init, deleteLast ((== p) . transActive) tx) code
    | p <- procs ]
  where
  procs = Set.elems $ traceProcs tc
  deleteLast pred l = reverse $ deleteBy (const pred) undefined $ reverse l

counterexample' :: Testable prop => String -> prop -> Property
counterexample' s =
  QCP.callback $ QCP.PostTest QCP.Counterexample $ \st res ->
    when (QCP.ok res == Just False) $ do
      res <- QCE.tryEvaluateIO (QCT.putLine (QCS.terminal st) s)
      case res of
        Left err ->
          QCT.putLine (QCS.terminal st) (QCP.formatException "Exception thrown while printing test case" err)
        Right () ->
          return ()

tracePropS :: (Testable p) => (AUTOSAR a -> Gen (a, Trace)) -> AUTOSAR a -> (Trace -> p) -> Property
tracePropS sim code prop = property $ sized $ \n -> do
  let limit = (1+n*10)
      gen :: Gen Trace
      gen = fmap (limitTrans limit . snd) $ sim code
  unProperty $ forAllShrink gen (shrinkTrace code) prop

traceProp :: (Testable p) => AUTOSAR a -> (Trace -> p) -> Property
traceProp code prop = tracePropS simulationRandG code prop

limitTrans :: Int -> Trace -> Trace
limitTrans t (a,trs) = (a,take t trs)

limitTime :: Time -> Trace -> Trace
limitTime t (a,trs) = (a,limitTimeTrs t trs) where
  limitTimeTrs t _ | t < 0                 = []
  limitTimeTrs t (del@Trans{transLabel = DELTA d}:trs) = del:limitTimeTrs (t-d) trs
  limitTimeTrs t []                        = []
  limitTimeTrs t (x:xs)                    = x : limitTimeTrs t xs

printLogs :: Trace -> IO Trace
printLogs trace = do
    mapM_ (\(id,v) -> putStrLn (id ++ ":" ++ show v)) $ traceLogs trace
    return trace

debug :: Trace -> IO ()
debug = mapM_ print . traceLabels

data Measure a = Measure { measureID    :: ProbeID
                         , measureTime  :: Time
                         , measureTrans :: Int
                         , measureValue :: a
                         } deriving (Functor, Show)

measureTimeValue :: Measure t -> (Time, t)
measureTimeValue m = (measureTime m, measureValue m)

-- Gets all measured values with a particular probe-ID and type
probe :: Data a => ProbeID -> Trace -> [Measure a]
probe pid t = internal $ probes' [pid] t

-- Get string representations of all measured values with a particular probe-ID
probeString :: ProbeID -> Trace -> [Measure String]
probeString pid t = map (fmap show) $ probes' [pid] t

-- Get all measured values for a set of probe-IDs and a type
probes :: Data a => [ProbeID] -> Trace -> [Measure a]
probes pids t = internal $ probes' pids t

probes' :: [ProbeID] -> Trace -> [Measure Value]
probes' pids t = concat $ go 0 0.0 (traceTrans t)
  where
    go n t (Trans{transLabel = DELTA d}:labs)  = go (n+1) (t+d) labs
    go n t (tr:trs)         =  probes : logs : go (n+1) t trs
      where probes          = [ Measure i t n v | (Just v,i) <- filtered (transLabel tr) ]
            logs            = [ Measure i t n v | (i,v) <- transLogs tr, i `elem` pids ]
    go _ _ _                = []
    ps                      = [ p | p <- traceProbes t, probeID p `elem` pids ]
    filtered lab            = [ (runProbe p lab, probeID p) | p <- ps ]

internal :: Data a => [Measure Value] -> [Measure a]
internal ms = [m{measureValue = a}|m <- ms, Just a <- return (value (measureValue m))]

-------------------------------------------------------------------------------
-- * Simulation with external connections
-------------------------------------------------------------------------------

-- * Communications protocol.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Transmission during simulation for one round of communications are performed
-- according to:
--
-- 1) Receive 1 byte from sender. If this byte is 0, we're ok, otherwise,
--    the sender has requested a halt.
-- 2) Unless the sender has requested a halt, receive a double from the sender
--    (i.e. 8 bytes on a 64-bit machine). This is the current sample time.
-- 3) Receive @n@ bytes from the sender, building the vector of inputs, agreed
--    upon before the start of the simulation with 'handshake'.
-- 4) Process data. If successful, send 1 byte to the sender according to the
--    same protocol as in (1), followed by @n@ bytes of new data. If
--    unsuccessful, send 1 non-zero byte.
--

data Status = OK | DIE
  deriving Show

-- | Write a status on the file descriptor.
writeStatus :: MonadIO m => Status -> Fd -> m ()
writeStatus status fd = liftIO $
  do bc <- fdWrite fd $ return $ chr $
             case status of
               OK  -> 0
               DIE -> 1
     unless (bc == 1) $ fail $
       "writeStatus: tried to write 1 byte, but succeeded with " ++ show bc

-- | Read a status from the input file descriptor.
readStatus :: MonadIO m => Fd -> m Status
readStatus fd = liftIO $
  do (s, bc) <- fdRead fd 1
     when (bc /= 1) $ fail $
       "readStatus: expected 1 byte, got " ++ show bc
     return $
       case ord (head s) of
         0 -> OK
         _ -> DIE

-- | Transfer information about desired port widths and port labels.
handshake :: (Fd, Fd) -> (Int, Int, [String], [String]) -> IO ()
handshake (fdIn, fdOut) (widthIn, widthOut, labelsIn, labelsOut) =
  do status <- readStatus fdIn
     case status of
      OK ->
        do -- Transfer port widths 
           checkedFdWrite fdOut [chr widthIn]
           checkedFdWrite fdOut [chr widthOut]

           -- Transfer labels
           sendLabels fdOut labelsIn
           sendLabels fdOut labelsOut
           return ()

      _ -> fail "Error when performing handshake."

-- | Transfers a list of string labels during the handshake.
sendLabels :: Fd -> [String] -> IO ()
sendLabels fd ls = 
  do -- Transfer input label count
     checkedFdWrite fd [chr (length ls)]
     forM_ ls $ \label ->
       withCStringLen label $ \(cstr, len) ->
         do checkedFdWrite fd [chr len] -- Send label length
            bc <- fdWriteBuf fd (castPtr cstr) (fromIntegral len) 
            when (bc /= fromIntegral len) $ fail $
              "sendLabels: tried to write " ++ show len ++ " bytes, but " ++
              "succeeded with " ++ show bc

-- | Performs a write with the desired byte count, calls @fail@ if it did not 
-- succeed.
checkedFdWrite :: Fd     -- ^ File descriptor
               -> String -- ^ String to send
               -> IO ()
checkedFdWrite fd str =
  do bc <- fdWrite fd str
     when (fromIntegral bc /= length str) $ fail $ 
       "checkedFdWrite: tried sending " ++ show (length str) ++ " bytes, " ++
       "sent " ++ show bc

-- Log information on @stderr@.
logWrite :: MonadIO m => String -> m () 
logWrite str = liftIO $ hPutStrLn stderr $ "[ARSIM] " ++ str 

-- * @External@ typeclass. 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Typeclass for marking DataElements as external connections. 

-- | A type class for marking values of type @a@ carrying an address for export.
-- @fromExternal@ carries input /from/ Simulink, and @toExternal@ /to/ Simulink.
-- The @String@ in the tuple is intended to provide port labels for Simulink.
class External a where
  fromExternal :: a -> [(Address, String)]
  fromExternal _ = []

  toExternal :: a -> [(Address, String)]
  toExternal _ = []
  {-# MINIMAL fromExternal | toExternal #-}

instance {-# OVERLAPPABLE #-} External a => External [a] where
  fromExternal = concatMap fromExternal 
  toExternal   = concatMap toExternal 

instance {-# OVERLAPPABLE #-} (External a, External b) => External (a, b) where
  fromExternal (a, b) = fromExternal a ++ fromExternal b
  toExternal   (a, b) = toExternal   a ++ toExternal   b

instance External (DataElement q a r c) where
  fromExternal de = [(address de, "DATAELEMENT_FROM")]
  toExternal   de = [(address de, "DATAELEMENT_TO")]

-- Some helpers to assist with the labeling. This system turned out to be not
-- so practical; might change it later. In the meantime we get automatic labels
-- in Simulink at least (to some extent)

-- | Add a tailing number to an address-label combination.
addNum :: Int -> [(Address, String)] -> [(Address, String)]
addNum num = map (\(a, l) -> (a, l ++ show num))

-- | Replace the label of an address-label combination.
relabel :: String -> [(Address, String)] -> [(Address, String)]
relabel str = map (\(a, _) -> (a, str)) 

-- * Marshalling data.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Some helper functions for communicating data as bytes over named pipes.

sizeOfDouble :: ByteCount
sizeOfDouble = fromIntegral (sizeOf (undefined :: Double))

mkCDouble :: Double -> CDouble
mkCDouble = CDouble

mkCDoubleEnum :: Enum a => a -> CDouble
mkCDoubleEnum = mkCDouble . fromIntegral . fromEnum

fromCDouble :: CDouble -> Double
fromCDouble (CDouble d) = d

-- | @'sendCDouble' x fd@ sends the double @x@ to the file descriptor @fd@.
sendCDouble :: MonadIO m => CDouble -> Fd -> m ()
sendCDouble x fd = liftIO $
  with x $ \ptr ->
    let bufPtr = castPtr ptr
    in do bc <- fdWriteBuf fd bufPtr sizeOfDouble
          unless (bc == sizeOfDouble) $ fail $
            "sendCDouble: Tried sending " ++ show sizeOfDouble ++ 
            " bytes but succeeded with " ++ show bc ++ " bytes."
          return ()

-- | @'receiveCDouble' fd@ reads a double from the file descriptor @fd@.
receiveCDouble :: MonadIO m => Fd -> m CDouble
receiveCDouble fd = liftIO $
  allocaBytes (fromIntegral sizeOfDouble) $ \ptr ->
    let bufPtr = castPtr ptr
    in do fdReadBuf fd bufPtr sizeOfDouble
          peek ptr

-- | @'sendVector' sv fd@ sends the vector @sv@ to the file descriptor @fd@.
sendVector :: MonadIO m => SV.Vector CDouble -> Fd -> m ()
sendVector sv fd = liftIO $
  do mv <- SV.thaw sv
     MSV.unsafeWith mv $ \ptr ->
       let busWidth = sizeOfDouble * fromIntegral (SV.length sv)
           busPtr   = castPtr ptr
       in do bc <- fdWriteBuf fd busPtr busWidth
             unless (fromIntegral bc == busWidth) $
               fail $ "sendVector: tried sending " ++ show busWidth ++
                      " bytes but succeeded with " ++ show bc ++ " bytes."

-- | @'receiveVector' fd width@ reads @width@ doubles from the file descriptor
-- @fd@ and returns a storable vector.
receiveVector :: MonadIO m => Fd -> Int -> m (SV.Vector CDouble)
receiveVector fd width = liftIO $
  do mv <- MSV.new width
     MSV.unsafeWith mv $ \ptr ->
       let busWidth = sizeOfDouble * fromIntegral width
           busPtr   = castPtr ptr
       in do bc <- fdReadBuf fd busPtr busWidth
             unless (bc == busWidth) $
               fail $ "receiveVector: expected " ++ show busWidth ++ " bytes" ++
                      " but read " ++ show bc ++ " bytes."
     SV.freeze mv

-- * Process/vector conversions.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Facilities for converting between external data (as storable vectors) and
-- internal data (i.e. Input/Output processes).

-- | This might be unnecessary and should probably be avoided.
copyVector :: SV.Vector CDouble -> RandStateIO (SV.Vector CDouble)
copyVector vec = liftIO $ SV.freeze =<< SV.thaw vec

-- | Convert a list of processes to a vector for marshalling.
procsToVector :: ProcMap             -- ^ List of /all/ processes in the model.
              -> SV.Vector CDouble   -- ^ Copy of previous output bus
              -> Map Address Int     -- ^ Address to vector index
              -> SV.Vector CDouble
procsToVector ps prev idx = prev // es
  where
    es = [ (fromJust $ Map.lookup a idx, castValue v) 
         | Output a v <- pmapElems ps
         ]

-- | Cast some members of the Value type to CDouble.
castValue :: Value -> CDouble
castValue x =
  let v1      = value x :: Maybe Bool
      v2      = value x :: Maybe Integer
      v3      = value x :: Maybe Double
      failure = error "Supported types for export are Bool, Integer and Double."
  in maybe (maybe (maybe failure mkCDoubleEnum v1)
                                 mkCDoubleEnum v2)
                                 mkCDouble     v3

-- | Convert a vector to a list of processes.
vectorToProcs :: SV.Vector CDouble
              -> SV.Vector CDouble
              -> Map Int Address
              -> [Update Proc]
vectorToProcs vec prev idx = map toProc $ filter diff es
  where
    es = [0..] `zip` SV.toList vec

    diff (i, x)
      | (prev ! i) /= x = True
      | otherwise       = False

    toProc (i, x) = Update (Input addr val)
      where
        addr = fromJust $ Map.lookup i idx
        val  = toValue $ fromCDouble x

-- * Simulator with external connections.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A variant of the 'simulate' which exports some addresses for communication
-- over named pipes.
--
-- Provides some basic facilites for running the simulator inside the IO monad,
-- such as a IO state.

-- | External simulator state.
data RandState = RandState
  { gen     :: StdGen
  , prevIn  :: SV.Vector CDouble
  , prevOut :: SV.Vector CDouble
  , addrIn  :: Map Int Address
  , addrOut :: Map Address Int
  }

-- | Initial external simulator state. Currently fixed to random scheduling.
rstate0 :: StdGen -> RandState
rstate0 g = RandState
  { gen     = g
  , prevIn  = SV.empty
  , prevOut = SV.empty
  , addrIn  = Map.empty
  , addrOut = Map.empty
  }

type RandStateIO = StateT RandState IO

-- | Forcing the @randomSched@ scheduler to live in IO.
ioRandomSched :: Scheduler RandStateIO
ioRandomSched alts =
 do (n, g) <- (next . gen) <$> get
    modify (\st -> st { gen = g})
    let (label, active, logs, procs) = alts !! (n `mod` length alts)
    return $ Just (Trans n label active logs, procs)

-- | Initialize the simulator with an initial state and run it. This provides
-- the same basic functionality as 'simulation'.
simulationExt :: (Fd, Fd)                      -- ^ (Input, Output)
              -> AUTOSAR a                     -- ^ AUTOSAR system.
              -> [(Int, Address)]              -- ^ ...
              -> [(Address, Int)]              -- ^ ...
              -> RandStateIO (a, Trace)
simulationExt fds sys idx_in idx_out =
  do -- Initialize state.
     modify $ \st ->
       st { prevIn  = SV.replicate (length idx_in) (1/0)
          , prevOut = SV.replicate (length idx_out) 0.0
          , addrIn  = Map.fromList idx_in
          , addrOut = Map.fromList idx_out
          }

     let procs1        = pmapFromList (procs state1 ++ outs)
         (res, state1) = initialize sys
         a `conn` b    = (a, b) `elem` conns state1 || a==b
         outs          = [ Output a (toValue (0.0 :: Double)) 
                         | (a,i) <- idx_out ]
     
     trs <- simulateExt fds ioRandomSched conn procs1
     return (res, (state1, trs))

-- | Internal simulator function. Blocks until we receive input from the
-- input file descriptor, which drives the simulation forward.
simulateExt :: (Fd, Fd)                          -- ^ (Input, Output)
         -> Scheduler RandStateIO
         -> ConnRel
         -> ProcMap
         -> RandStateIO [Transition]
simulateExt (fdInput, fdOutput) sched conn procs =
  do status <- readStatus fdInput
     case status of
       OK ->
         do -- Calling length instead of just storing port widths is silly:
            vec <- receiveVector fdInput . SV.length =<< gets prevIn

            RandState { prevIn = prev1, addrIn = addr_in } <- get
            let extProcs = vectorToProcs vec prev1 addr_in
                newProcs = pmapUpdate procs extProcs 
            
            -- Re-set the previous input to the current input. Not sure if
            -- we have to /copy/ these vectors (they are storable) or if GHC
            -- figures it out for us (i.e. will they just reassign the pointer?)
            newPrevIn <- copyVector vec
            modify $ \st -> st { prevIn = newPrevIn }

            progress <- simulate1Ext sched conn newProcs []
            case progress of
              Nothing ->
                do logWrite "Ran out of alternatives, requesting halt."
                   writeStatus DIE fdOutput
                   return []

              Just (dt, procs1, ts) ->
                do RandState { addrOut = addr_out, prevOut = prev2 } <- get
                  
                   -- Produce an output vector.
                   let next   = mkCDouble dt
                       procs2 = pmapUpdate procs procs1
                       output = procsToVector procs2 prev2 addr_out

                   -- Re-set the previous output to the current output.
                   newPrevOut <- copyVector output
                   modify $ \st -> st { prevOut = newPrevOut }

                   -- Signal OK and then data.
                   writeStatus OK fdOutput
                   when (next < 1e-3) $ logWrite $
                     "Warning: Small delta step of " ++ show next ++ " sent."
                   sendCDouble next fdOutput
                   sendVector output fdOutput

                   (ts++) <$> simulateExt (fdInput, fdOutput) sched conn procs2

       -- In case this happened we did not receive OK and we should die.
       DIE ->
         do logWrite "Master process requested halt, stopping."
            return []

-- | @simulate1Ext@ progresses the simulation as long as possible without
-- advancing time. When @maximumProgress@ returns a @DELTA@ labeled
-- transition, @simulate1Ext@ returns @Just (time, procs, transitions)@. If the
-- simulator runs out of alternatives, @Nothing@ is returned.
simulate1Ext :: Scheduler RandStateIO
             -> ConnRel
             -> ProcMap
             -> [Transition]
             -> RandStateIO (Maybe (Time, ProcMap, [Transition]))
simulate1Ext sched conn procs acc
  | null alts = return Nothing
  | otherwise =
    do mtrans <- maximumProgress sched alts
       case mtrans of
         -- The trace finished - should we return a Just here?
         Nothing -> return $ error "The trace finished. I don't know what to do."
         Just (trans, procs1) ->
           case pmapUpdate procs procs1 of 
             procs2 -> case transLabel trans of
               DELTA dt -> 
                 return $ Just (dt, procs2, trans:acc)
               _ -> 
                 simulate1Ext sched conn procs2 (trans:acc)
  where
    alts = step conn procs

-- * Simulation entry-points.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Provides entry-points for both the internal and external simulator functions.
--
-- Given some AUTOSAR system @sys :: AUTOSAR a@ exporting some data structure
-- containing references to the data elements we wish to connect to external
-- software, an entry point can be created using
--
-- > main :: IO ()
-- > main = simulateUsingExternal sys
--
-- Or likewise, we could run an internal simulation (provided the system is
-- self-contained). This example limits execution at @5.0@ seconds, using the
-- random scheduler for scheduling and calls @makePlot@ on the resulting trace.
--
-- > main :: IO ()
-- > main = simulateStandalone 5.0 makePlot (RandomSched (mkStdGen 111)) sys

-- | Use this function to create a runnable @main@ for the simulator software
-- when running the simulator standalone.
simulateStandalone :: Time             -- ^ Time limit
                   -> (Trace -> IO a)  -- ^ Trace processing function
                   -> SchedChoice      -- ^ Scheduler choice
                   -> AUTOSAR b        -- ^ AUTOSAR system
                   -> IO a
simulateStandalone time f sched = f . limitTime time . execSim sched

{- There are two ways to run the Simulink model. One way is to run the simulation
 - from Simulink directly (for example by using the MATLAB gui). In this case
 - it's the Simulink C stub that creates named pipes and launches the
 - Haskell AUTOSAR simulator (handled by simulateUsingExternal).
 - The second way is to run the Haskell 'driver', which sets up the
 - environment (the named pipes) and launches the Simulink model (simulateDriveExternal).
 - The Simulink C stub realizes that it should not set up the environment
 - by looking at the environment variable ARSIM_DRIVER. -}

-- | Use this function to create a runnable @main@ for the simulator software
-- when connecting with external software, i.e. Simulink.
simulateUsingExternal :: External a => AUTOSAR a -> IO ()
simulateUsingExternal sys = exceptionHandler $
  do args <- getArgs
     case args of
      [inFifo, outFifo] -> runWithFIFOs inFifo outFifo sys
      _ ->
        do logWrite $ "Wrong number of arguments. Proceeding with default " ++
                      "FIFOs."
           runWithFIFOs "/tmp/infifo" "/tmp/outfifo" sys

-- | Run the simulation with external software (i.e. Simulink) by
-- starting the external component from Haskell, and clean up afterwards.
simulateDriveExternal :: External a => FilePath -> AUTOSAR a -> IO ()
simulateDriveExternal ext sys = exceptionHandler $
  do args <- getArgs
     (inFifo, outFifo) <- case args of
       [inFifo, outFifo] -> return (inFifo, outFifo)
       _ ->
          do logWrite $ "Wrong number of arguments. Proceeding with default " ++
                        "FIFOs."
             return ("/tmp/infifo", "/tmp/outfifo")
     bracket (createNamedPipe inFifo accessModes)
             (const $ removeFile inFifo) $ \_ -> do
       bracket (createNamedPipe outFifo accessModes)
               (const $ removeFile outFifo) $ \_ -> do
         cur_env <- getEnvironment
         let procSpec = (proc ext []) { env = Just $ cur_env ++ [("ARSIM_DRIVER", "")] }
         bracket (createProcess procSpec) (\ (_, _, _, h) -> terminateProcess h) $ \_ -> do
           runWithFIFOs inFifo outFifo sys

-- | The external simulation entry-point. Given two file descriptors for
-- input/output FIFOs we can start the simulation of the AUTOSAR program.
entrypoint :: External a
           => AUTOSAR a                      -- ^ AUTOSAR program.
           -> (Fd, Fd)                       -- ^ (Input, Output)
           -> IO ()
entrypoint system fds =
  do -- Fix the system, initialize to get information about AUTOSAR components
     -- so that we can pick up port adresses and all other information we need.
     let (res, _)               = initialize system
         (addr_in, labels_in)   = unzip (fromExternal res)
         (addr_out, labels_out) = unzip (toExternal res)
         inwidth                = length addr_in
         outwidth               = length addr_out
         
         -- These maps need to go with the simulator as static information
         idx_in  = zip [0..] addr_in
         idx_out = zip addr_out [0..]

     -- Perform the handshake: Transfer port widths and labels
     handshake fds ( inwidth
                   , outwidth
                   , labels_in 
                   , labels_out 
                   )

     runStateT (simulationExt fds system idx_in idx_out)
               (rstate0 (mkStdGen 111))
     return ()

-- | Run simulation of the system using the provided file descriptors as
-- FIFOs.
runWithFIFOs :: External a
             => FilePath
             -> FilePath
             -> AUTOSAR a
             -> IO ()
runWithFIFOs inFifo outFifo sys =
  do logWrite $ "Input FIFO:  " ++ inFifo
     logWrite $ "Output FIFO: " ++ outFifo
     fdInput  <- openFd inFifo  ReadOnly Nothing defaultFileFlags
     fdOutput <- openFd outFifo WriteOnly Nothing defaultFileFlags
     entrypoint sys (fdInput, fdOutput)

-- Exception handling for 'simulateUsingExternal'.
exceptionHandler :: IO () -> IO ()
exceptionHandler = catchPure . catchEOF
  where
    catchEOF m = catchIf isEOFError m $ \_ ->
      logWrite "Master process closed pipes."
    catchPure m = catchAll m $ \e ->
      logWrite $ "Caught " ++ show e

-- Code below this point is a bit outdated.


type Measurement a           = [((Int,Time),a)] -- The Int is the number of transitions

-- Gets ALL probes of a certain type, categorized by probe-ID.
-- This function is strict in the trace, so limitTicks and/or limitTime should be used for infinite traces.
probeAll :: Data a => Trace -> [(ProbeID,Measurement a)]
probeAll t = [(s,m') |(s,m) <- probeAll' t, let m' = internal' m, not (null m') ]

internal' :: Data a => Measurement Value -> Measurement a
internal' ms = [(t,a) | (t,v) <- ms, Just a <- return (value v)]

probeAll'                   :: Trace -> [(ProbeID,Measurement Value)]
probeAll' (state,trs)   = Map.toList $ Map.fromListWith (flip (++)) collected
  where collected       = collect (simProbes state) 0.0 0 trs ++ collectLogs 0.0 0 trs


collect :: [Probe] -> Time -> Int -> [Transition] -> [(ProbeID,Measurement Value)]
collect probes t n []       = []
collect probes t n (Trans{transLabel = DELTA d}:trs)
                            = collect probes (t+d) (n+1) trs
collect probes t n (Trans{transLabel = label}:trs)
                            = measurements ++ collect probes t (n+1) trs
  where measurements        = [ (s,[((n,t),v)]) | (s,f) <- probes, Just v <- [f label] ]


collectLogs t n []          = []
collectLogs t n (Trans{transLabel = DELTA d}:trs)
                            = collectLogs (t+d) (n+1) trs
collectLogs t n (Trans{transLogs = logs}:trs)
                            = [ (i,[((n,t),v)]) | (i,v) <- logs ] ++ collectLogs t (n+1) trs


