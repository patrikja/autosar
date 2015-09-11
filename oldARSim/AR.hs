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

{-# LANGUAGE FlexibleInstances #-}
module AR where
import qualified Data.Map as Map
import Data.Map (Map, keys, elems)
import Data.List
import Data.Maybe

dom :: Map a b -> [a]
rng :: Map a b -> [b]
dom = keys
rng = elems

a `subsetof` b      = all (`elem` b) a
a `equal` b         = a `subsetof` b && b `subsetof` a

unique xs           = xs == nub xs

data Component =
      Atomic      (Map PortName Port) (Map VarName Type)       [Runnable]
    | Composition (Map PortName Port) (Map CompName Component) [Connector]

ports (Atomic iface _ _)        = iface
ports (Composition iface _ _)   = iface

data PortKind =
      SenderReceiver (Map VarName Type)   
    | ClientServer   (Map OpName Operation)  -- a sum (choice) of operations
    
instance Eq PortKind where
    SenderReceiver ts == SenderReceiver ts'  =  ts == ts'
    ClientServer ops  == ClientServer ops'   =  ops == ops'
    _ == _          = False

data Port = Provided PortKind
          | Required PortKind
    deriving Eq

inverse (Provided p)    = Required p
inverse (Required p)    = Provided p

data Operation =
    Op (Map VarName ArgType) [Int]  -- ^ a product (tuple) of arguments, set of error codes

instance Eq Operation where
    Op args errs == Op args' errs'  = args == args' && errs == errs'
    
data ArgType   = 
    In    Type
  | InOut Type
  | Out   Type
    deriving Eq

type Connector  = (PortRef, PortRef)    

data PortRef = 
    Qual CompName PortName
  | Ext PortName
    deriving Eq

type PortName   = String
type CompName   = String
type FieldName  = String
type TagName    = String
type OpName     = String
type VarName    = String

type Statevars  = Map VarName Value
type Arguments  = Map VarName Value
type Response   = Either Int (Map VarName Value)

-- | Four different kinds of |Runnable|s
data Runnable = 
    Init Behavior    -- ^ should set up initial state etc.
  | DataReceived     PortName         (Statevars -> Arguments -> Behavior)
    -- ^ async. method
  | OperationInvoked PortName OpName  (Statevars -> Arguments -> Behavior)
    -- ^ sync. operation
  | Timing           Double           (Statevars -> Behavior)
    -- ^ repeated execution

{- There are different classes of runnables. This type is not covering
all of them. For example, in the full AUTOSAR standard, a runnable
could have a set of waitPoints, but this simplified description
assumes this set is empty.  -}


-- | Observable effect of a |Runnable|
data Behavior = 
    Sending   PortName Arguments Behavior 
    -- ^ 
  | Invoking  PortName OpName Arguments (Response -> Behavior) 
    -- ^ Invoke operation (must check "port matching")
  | Responding Response Behavior
    -- ^ Act. on a call (might be better to merge with Idling)
  | Idling Statevars
  | Crashed
  | Comp (Map CompName Behavior)

data Observation =
    Send    PortRef Arguments
  | Invoke  PortRef OpName Arguments
  | Respond Response
  | Tau

update :: k -> v -> Map k v -> Map k v
update       = undefined

connections :: env -> CompName -> PortName -> [PortRef]
connections  = undefined

runnables :: env -> CompName -> [Runnable]
runnables    = undefined

data Msg = Msg CompName PortName Arguments deriving Eq

type ComponentLinks = [(CompName, CompName)]

-- | perhaps a queue or at least timestamped messages
type MsgPool = [Msg] 

type State = (Map CompName Behavior, MsgPool, ComponentLinks)

reduce :: env -> State -> [(State, Observation)]
reduce env (bhvs, msgs, links) = 

    [ ((update c next bhvs, Msg c' p' args : msgs, links), Tau)
    | (c,Sending p args next) <- Map.assocs bhvs
    , Qual c' p' <- connections env c p
    ] ++
    
    [ ((update c (run svars args) bhvs, delete (Msg c p args) msgs, links), Tau) 
    | (c,Idling svars) <- Map.assocs bhvs
    , DataReceived p run <- runnables env c
    , Msg c1 p1 args <- msgs
    , c == c1, p == p1
    ] ++
    
    [ ((update c' (run svars args) bhvs, msgs, (c,c'):links), Tau) 
    | (c,Invoking p op args cont) <- Map.assocs bhvs
    , (c',Idling svars) <- Map.assocs bhvs
    , OperationInvoked p' op' run <- runnables env c'
    , Qual c' p' `elem` connections env c p
    ] ++
    
    [ ((update c (cont resp) (update c' next bhvs), msgs, delete (c,c') links), Tau) 
    | (c,Invoking p op args cont) <- Map.assocs bhvs
    , (c',Responding resp next) <- Map.assocs bhvs
    , (c,c') `elem` links
    ] ++
    
    [ ((update c next bhvs, msgs, links), Send (Ext p') args) 
    | (c,Sending p args next) <- Map.assocs bhvs
    , Ext p' <- connections env c p
    ] ++

    [ ((update c (Invoking p op args cont) bhvs, msgs, links), Invoke (Ext p') op args) 
    | (c,Invoking p op args cont) <- Map.assocs bhvs
    , Ext p' <- connections env c p
    ] ++

    [ ]

--------------------------------------------------------------
            
validComp partial (Atomic iface sig runns)
                            = unique (dom iface) &&
                              unique int_rcvs && 
                              unique int_ops &&
                              int_rcvs `subsetof` ext_rcvs && int_ops `subsetof` ext_ops &&
                              (partial || total)
  where ext_rcvs            = [ p | (p, Provided (SenderReceiver _)) <- Map.assocs iface ]
        ext_ops             = [ (p,o) | (p, Provided (ClientServer ops)) <- Map.assocs iface, o <- dom ops ]
        int_rcvs            = [ p | DataReceived p _ <- runns ]
        int_ops             = [ (p,o) | OperationInvoked p o _ <- runns ]
        total               = ext_rcvs `subsetof` int_rcvs && ext_ops `subsetof` int_ops
validComp partial (Composition iface comps conns)
                            = unique (dom iface) &&
                              unique (dom comps) && 
                              all (validComp partial) (rng comps) &&
                              all (validConn all_ports) conns &&
                              all (== 1) (map fan_out req_clisrv) &&
                              all (>= 1) (map fan_out req_sndrcv)
  where all_ports           = [ (Qual c p, port) 
                              | (c,comp) <- Map.assocs comps, (p,port) <- Map.assocs $ ports comp ] ++
                              [ (Ext p, inverse port) 
                              | (p,port) <- Map.assocs iface ]
        req_sndrcv          = [ r | (r,Required (SenderReceiver _)) <- all_ports ]
        req_clisrv          = [ r | (r,Required (ClientServer _)) <- all_ports ]
        fan_out r           = length [ q | (p,q) <- conns, p == r ]

validConn ports (r,r')      = case (lookup r ports, lookup r' ports) of
                                (Just (Required p), Just (Provided p')) -> p == p'
                                _ -> False


------------------------------------------------------------------

data Type =
    TInt                            
  | TBool                           
  | TChar                           
  | TArray  Type                    
  | TStruct (Map FieldName Type)    
  | TUnion  (Map TagName Type)

instance Eq Type where
    TInt == TInt                    = True
    TBool == TBool                  = True
    TChar == TChar                  = True
    TArray t == TArray t'           = t == t'
    TStruct ts == TStruct ts'       = ts == ts'
    TUnion ts == TUnion ts'         = ts == ts'
    _ == _                          = False

data Value =
    VInt Int                        
  | VBool Bool                      
  | VChar Char                      
  | VArray [Value]                  
  | VStruct (Map FieldName Value)   
  | VUnion TagName Value
    deriving Eq

hasType TInt           (VInt _)        = True
hasType TBool          (VBool _)       = True
hasType TChar          (VChar _)       = True
hasType (TArray t)     (VArray vs)     = all (hasType t) vs
hasType (TStruct ts)   (VStruct fs)    = unique (dom fs) && dom ts `equal` dom fs &&
                                         and [ hasTypeIn ts f v | (f,v) <- Map.assocs fs ]
hasType (TUnion ts)    (VUnion tag v)  = hasTypeIn ts tag v

hasTypeIn ts k v = case Map.lookup k ts of 
                     Just t  -> hasType t v; 
                     _       -> False

