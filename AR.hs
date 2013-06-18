{-# LANGUAGE FlexibleInstances #-}
module AR where

import Data.List
import Data.Maybe

type Map a b        = [(a,b)]

dom                 = map fst
rng                 = map snd

a `subsetof` b      = all (`elem` b) a
a `equal` b         = a `subsetof` b && b `subsetof` a

unique xs           = xs == nub xs

data Component =
    Atomic (Map PortName Port) (Map VarName Type) [Runnable]
    |
    Composition (Map PortName Port) [(CompName,Component)] [Connector]

ports (Atomic iface _ _)        = iface
ports (Composition iface _ _)   = iface

data PortKind =
    SenderReceiver (Map VarName Type)   
    |
    ClientServer (Map OpName Operation)
    
instance Eq PortKind where
    SenderReceiver ts == SenderReceiver ts' 
                    = ts `equal` ts'
    ClientServer ops == ClientServer ops'   
                    = ops `equal` ops'
    _ == _          = False

data Port =
    Provided PortKind |
    Required PortKind
    deriving Eq

inverse (Provided p)    = Required p
inverse (Required p)    = Provided p

data Operation =
    Op (Map VarName ArgType) [Int]

instance Eq Operation where
    Op args errs == Op args' errs'  = args `equal` args' && errs `equal` errs'
    
data ArgType   = 
    In    Type  |
    InOut Type  |
    Out   Type
    deriving Eq

type Connector  = (PortRef, PortRef)    

data PortRef = 
    Qual CompName PortName |
    Ext PortName
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

data Runnable =
    Init Behavior
    |
    DataReceived PortName (Statevars -> Arguments -> Behavior)
    |
    OperationInvoked PortName OpName (Statevars -> Arguments -> Behavior)
    |
    Timing Double (Statevars -> Behavior)

data Behavior =
    Sending PortName Arguments Behavior 
    |
    Invoking PortName OpName Arguments (Response -> Behavior) 
    |
    Responding Response Behavior
    |
    Idling Statevars
    |
    Crashed
    |
    Comp (Map CompName Behavior)

data Observation =
    Send PortRef Arguments
    |
    Invoke PortRef OpName Arguments
    |
    Respond Response
    |
    Tau

--------------------------------------------------------------
            
validComp partial (Atomic iface sig runns)
                            = unique (dom iface) &&
                              unique int_rcvs && 
                              unique int_ops &&
                              int_rcvs `subsetof` ext_rcvs && int_ops `subsetof` ext_ops &&
                              (partial || total)
  where ext_rcvs            = [ p | (p, Provided (SenderReceiver _)) <- iface ]
        ext_ops             = [ (p,o) | (p, Provided (ClientServer ops)) <- iface, o <- dom ops ]
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
  where all_ports           = [ (Qual c p, port) | (c,comp) <- comps, (p,port) <- ports comp ] ++
                              [ (Ext p, inverse port) | (p,port) <- iface ]
        req_sndrcv          = [ r | (r,Required (SenderReceiver _)) <- all_ports ]
        req_clisrv          = [ r | (r,Required (ClientServer _)) <- all_ports ]
        fan_out r           = length [ q | (p,q) <- conns, p == r ]

validConn ports (r,r')      = case (lookup r ports, lookup r' ports) of
                                (Just (Required p), Just (Provided p')) -> p == p'
                                _ -> False



data Type =
    TInt                            |
    TBool                           |
    TChar                           |
    TArray  Type                    |
    TStruct (Map FieldName Type)    |
    TUnion  (Map TagName Type)

instance Eq Type where
    TInt == TInt                    = True
    TBool == TBool                  = True
    TChar == TChar                  = True
    TArray t == TArray t'           = t == t'
    TStruct ts == TStruct ts'       = ts `equal` ts'
    TUnion ts == TUnion ts'         = ts `equal` ts'
    _ == _                          = False

data Value =
    VInt Int                        |
    VBool Bool                      |
    VChar Char                      |
    VArray [Value]                  |
    VStruct (Map FieldName Value)   |
    VUnion TagName Value
    deriving Eq

hasType TInt (VInt _)               = True
hasType TBool (VBool _)             = True
hasType TChar (VChar _)             = True
hasType (TArray t) (VArray vs)      = all (hasType t) vs
hasType (TStruct ts) (VStruct fs)   = unique (dom fs) && dom ts `equal` dom fs &&
                                      and [ hasTypeIn ts f v | (f,v) <- fs ]
hasType (TUnion ts) (VUnion tag v)  = hasTypeIn ts tag v

hasTypeIn ts k v                    = case lookup k ts of Just t -> hasType t v; _ -> False
