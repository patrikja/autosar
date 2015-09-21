{-
Copyright (c) 2014-2015, Johan Nordlander, Jonas Duregård, Michał Pałka,
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module NewARSim (module NewARSim, Typeable, Data, mkStdGen, StdGen) where

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
                            | NO_DATA
                            | NEVER_RECEIVED
                            | LIMIT
                            | UNCONNECTED
                            | TIMEOUT
                            | IN_EXCLUSIVE_AREA
                            deriving Show

newtype DataElement q a r c             = DE Address      -- Async channel of "a" data
newtype ClientServerOperation a b r c   = OP Address      -- Sync channel of an "a->b" service

data Queued         -- Parameter q above
data Unqueued

data Required       -- Parameter r above
data Provided

data UnqueuedSenderComSpec a    = UnqueuedSenderComSpec { initSend :: a }
data UnqueuedReceiverComSpec a  = UnqueuedReceiverComSpec { initValue :: a }
data QueuedSenderComSpec a      = QueuedSenderComSpec
data QueuedReceiverComSpec a    = QueuedReceiverComSpec { queueLength :: Int }
data ServerComSpec a b          = ServerComSpec { bufferLength :: Int }
data ClientComSpec              = ClientComSpec

newtype InterRunnableVariable a c   = IV Address
newtype ExclusiveArea c             = EX Address

type Time                   = Double

data Event c                = forall q a. DataReceivedEvent (DataElement q a Required c)
                            | Timed Time
                            | Init

data ServerEvent a b c      = OperationInvokedEvent (ClientServerOperation a b Provided c)

data Invocation             = Concurrent
                            | MinInterval Time
                            deriving (Eq)


-- Simulator state ------------------------------------------------------------

data SimState               = SimState {
                                    procs       :: [Proc],
                                    conns       :: [Conn],
                                    simProbes   :: [Probe],
                                    initvals    :: Map.Map Address Value,
                                    nextA       :: Address
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

type ProbeID                = String
type Probe                  = (ProbeID, Label -> Maybe Value)
probeID :: Probe -> ProbeID
probeID = fst
runProbe :: Probe -> Label -> Maybe Value
runProbe = snd


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

state0                      = SimState { procs = [], conns = [], simProbes = [], initvals = Map.empty, nextA = 0 }

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
    NewComponent            :: (forall c. AR c a) -> ARInstr c a
    NewConnection           :: Conn -> ARInstr c ()

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
                            = run (sys ()) (st { simProbes = (s,f) : simProbes st })
    run' (NewInit a v :>>= sys) st
                            = run (sys ()) (st { initvals = Map.insert a v (initvals st) })
    run' (NewComponent subsys :>>= sys) st
                            = let (a,st') = runAR subsys st in run (sys a) st'
    run' (NewConnection conn :>>= sys) st
                            = run (sys ()) (st { conns = conn : conns st })
    run' (Return a) st      = (a,st)

initialize                  :: (forall c . AR c a) -> (a,SimState)
initialize sys              = (a, st { procs = map (apInit (conns st) (initvals st)) (procs st) })
  where (a,st)              = runAR sys state0

-- Restricting connections ----------------------------------------------------

class Port p where
    type PComSpec p :: *
    type RComSpec p :: *
    connect  :: p Provided () -> p Required () -> AR c ()
--    delegate :: [p r ()] -> AR c (p r ())
    provide  :: PComSpec p -> AR c (p Provided c)
    require  :: RComSpec p -> AR c (p Required c)

instance Data a => Port (DataElement Unqueued a) where
    type PComSpec (DataElement Unqueued a) = UnqueuedSenderComSpec a
    type RComSpec (DataElement Unqueued a) = UnqueuedReceiverComSpec a
    connect (DE a) (DE b) = newConnection (a,b)
    provide s = do a <- newAddress;
                   newInit a (toValue (initSend s))
                   return (DE a)
    require s = do a <- newAddress;
                   newProcess (DElem a False (Ok (toValue (initValue s))))
                   return (DE a)

instance Port (DataElement Queued a) where
    type PComSpec (DataElement Queued a) = QueuedSenderComSpec a
    type RComSpec (DataElement Queued a) = QueuedReceiverComSpec a
    connect (DE a) (DE b) = newConnection (a,b)
    provide s = do a <- newAddress; return (DE a)
    require s = do a <- newAddress; newProcess (QElem a (queueLength s) []); return (DE a)

instance Port (ClientServerOperation a b) where
    type PComSpec (ClientServerOperation a b) = ServerComSpec a b
    type RComSpec (ClientServerOperation a b) = ClientComSpec
    connect (OP a) (OP b) = newConnection (a,b)
    provide s = do a <- newAddress; return (OP a)
    require s = do a <- newAddress;
                   newProcess (Op a []); -- result buffer allocation
                   return (OP a)
      -- There is a bufferLength in s, but it is unclear (in AUTOSAR)
      -- what is means: argument or result buffer length? Or both?

class Addressed a where
        address                     :: a -> Address

instance Addressed (InterRunnableVariable a c) where
        address (IV n)              = n

instance Addressed (DataElement q a r c) where
        address (DE n)              = n

instance Addressed (ClientServerOperation a b r c) where
        address (OP n)              = n


class Seal m where
        seal                :: m c -> m ()

instance Seal (DataElement q a r) where
        seal (DE a)         = DE a

instance Seal (ClientServerOperation a b r) where
        seal (OP a)         = OP a


-- Derived AR operations ------------------------------------------------------

interRunnableVariable       :: Data a => a -> AR c (InterRunnableVariable a c)
exclusiveArea               :: AR c (ExclusiveArea c)
runnable                    :: Invocation -> [Event c] -> RTE c a -> AR c ()
serverRunnable              :: (Data a, Data b) =>
                                Invocation -> [ServerEvent a b c] -> (a -> RTE c b) -> AR c ()
component                   :: (forall c. AR c a) -> AR c a

component c                 = singleton $ NewComponent c

newConnection c             = singleton $ NewConnection c
newAddress                  = singleton $ NewAddress
newProcess p                = singleton $ NewProcess p
newInit a v                 = singleton $ NewInit a v
newInit :: Address -> Value -> Program (ARInstr c) ()

interRunnableVariable val   = do a <- newAddress; newProcess (Irv a (toValue val)); return (IV a)
exclusiveArea               = do a <- newAddress; newProcess (Excl a Free); return (EX a)

runnable inv trig code      = do a <- newAddress
                                 mapM (newProcess . Timer a 0.0) periods
                                 newProcess (Run a 0.0 act 0 (Static watch inv code'))
  where periods             = [ t | Timed t <- trig ]
        watch               = [ a | DataReceivedEvent (DE a) <- trig ]
        act                 = if null [ Init | Init <- trig ] then Idle else Pending
        code'               = \dyn -> code >> return dyn

serverRunnable inv ops code = do a <- newAddress
                                 newProcess (Run a 0.0 act 0 (Static watch inv code'))
  where watch               = [ a | OperationInvokedEvent (OP a) <- ops ]
        act                 = Serving [] []
        code'               = fmap toValue . code . fromDyn

fromDyn                     :: Data a => Value -> a
fromDyn                     = value'


-- TODO: add Reading/Writing classes instead of Addressed?
probeRead                   :: (Data a, Addressed (e a c)) => String -> e a c -> AR c' ()
probeRead s x              = singleton $ NewProbe s g
  where
    g (RD b (Ok v))    | a==b    = Just v
    g (RCV b (Ok v))   | a==b    = Just v
    g (IRVR b (Ok v))  | a==b    = Just v
    g (RES b (Ok v))   | a==b    = Just v
    g _                         = Nothing
    a = address x


probeWrite                  :: (Data a, Addressed (e a c)) => String -> e a c -> AR c' ()
probeWrite s x            = singleton $ NewProbe s g
  where
    g (IRVW b v)     | a==b     = Just v
    g (WR b v)       | a==b     = Just v
    g (SND b v _)    | a==b     = Just v -- Not sure about these.
--    g (CALL b v _)   | a==b     = Just v
    g (RET b v)      | a==b     = Just v
    g _                     = Nothing
    a = address x
{-
probeWrite'                 :: (Data b, Data a, Addressed (e a c)) => String -> e a c -> AR c' ()
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
                            | NEW   Address
                            | TERM  Address
                            | TICK  Address
                            | DELTA Time
                            | VETO
                            deriving Show

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

labelAddress :: Label -> Maybe Address
labelAddress l = case l of
          ENTER a            -> Just a
          EXIT  a            -> Just a
          IRVR  a ret        -> Just a
          IRVW  a val        -> Just a
          RCV   a ret        -> Just a
          SND   a val ret    -> Just a
          RD    a ret        -> Just a
          WR    a val        -> Just a
          UP    a ret        -> Just a
          INV   a            -> Just a
          CALL  a val ret    -> Just a
          RES   a ret        -> Just a
          RET   a val        -> Just a
          NEW   a            -> Just a
          TERM  a            -> Just a
          TICK  a            -> Just a
          DELTA t            -> Nothing
          VETO               -> Nothing



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
        say' (SND _ _ res)  (Send _ _     :>>= cont)    = [RInst a c ex       (cont res)]
        say' (RD _ res)     (Read _       :>>= cont)    = [RInst a c ex       (cont (fromStdDyn res))]
        say' (WR _ _)       (Write _ _    :>>= cont)    = [RInst a c ex       (cont void)]
        say' (UP _ res)     (IsUpdated _  :>>= cont)    = [RInst a c ex       (cont (fromStdDyn res))]
        say' (INV _)        (Invalidate _ :>>= cont)    = [RInst a c ex       (cont void)]
        say' (CALL _ _ res) (Call _ _     :>>= cont)    = [RInst a c ex       (cont res)]
        say' (RES _    res) (Result _     :>>= cont)    = [RInst a c ex       (cont (fromStdDyn res))]
        say' (RET _ _)      (Return v)                  = [RInst a Nothing ex (return (toValue ()))]
        say' (TERM _)       (Return _)                  = []
        say' label          (Printlog i v :>>= cont)    = say' label (view (cont ()))


mayLog (RInst a c ex code)                              = mayLog' (view code)
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
       | trig conn a s  &&  a `notElem` cs                      = CALL a v void
       | trig conn a s                                          = CALL a v LIMIT
mayHear conn (RES a _)       (Op b (v:vs))     | a==b           = RES a (Ok v)
mayHear conn (RES a _)       (Op b [])         | a==b           = VETO  -- RES a NO_DATA
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

type Logs                   = [(ProbeID,Value)]

type SchedulerOption        = (Label, Logs, [Proc])
data Transition             = Trans { transChoice :: Int
                                    , transLabel  :: Label
                                    , transLogs   :: Logs
                                    , transProcs  :: [Proc]}
type Scheduler m            = [SchedulerOption] -> m Transition
type Trace                  = (SimState, [Transition])

traceLabels :: Trace -> [Label]
traceLabels = map transLabel . traceTrans

traceTrans :: Trace -> [Transition]
traceTrans = snd

traceProbes :: Trace -> [Probe]
traceProbes = simProbes . fst

traceLogs :: Trace -> Logs
traceLogs = concat . map transLogs . traceTrans


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
  -- This can be used to define all the other cases
  AnySched            :: Monad m => Scheduler m -> (forall a. m a -> a) -> SchedChoice


runSim                         :: SchedChoice -> (forall c . AR c a) -> (a,Trace)
runSim TrivialSched sys        = runIdentity (simulation trivialSched sys)
runSim RoundRobinSched sys     = evalState (simulation roundRobinSched sys) 0
runSim (RandomSched g) sys     = evalState (simulation randomSched sys) g
runSim (AnySched sch run) sys  = run (simulation sch sys)

execSim :: SchedChoice -> (forall c . AR c a) -> Trace
execSim sch sys = snd $ runSim sch sys

limitTrans :: Int -> Trace -> Trace
limitTrans t (a,trs) = (a,take t trs)

limitTime :: Time -> Trace -> Trace
limitTime t (a,trs) = (a,limitTimeTrs t trs) where
  limitTimeTrs t _ | t < 0                 = []
  limitTimeTrs t (del@(Trans{transLabel = DELTA d}):trs) = del:limitTimeTrs (t-d) trs
  limitTimeTrs t []                        = []
  limitTimeTrs t (x:xs)                    = x : limitTimeTrs t xs

printLogs :: Trace -> IO Trace
printLogs trace = do
    mapM_ (\(id,v) -> putStrLn (id ++ ":" ++ show v)) $ traceLogs trace
    return trace

debug :: Trace -> IO ()
debug = mapM_ print . traceLabels where



data Measure a = Measure { measureID    :: ProbeID
                         , measureTime  :: Time
                         , measureTrans :: Int
                         , measureValue :: a
                         } deriving (Functor, Show)

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



-- Code below this point is a bit outdated.


type Measurement a           = [((Int,Time),a)] -- The int is the number of transitions

-- Gets ALL probes of a certain type, categorized by probe-ID.
-- This function is strict in the trace, so limitTicks and/or limitTime should be used for infinite traces.
probeAll :: Data a => Trace -> [(ProbeID,Measurement a)]
probeAll t = [(s,m') |(s,m) <- probeAll' t, let m' = internal' m, not (null m') ]

internal' :: Data a => Measurement Value -> Measurement a
internal' ms = [(t,a)|(t,v) <- ms, Just a <- return (value v)]

probeAll'               :: Trace -> [(ProbeID,Measurement Value)]
probeAll' (state,trs)   = Map.toList $ Map.fromListWith (flip (++)) $ collected
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
