module AR where


data System = 
    System {
        root            :: Component
    }
    
data Component =
    Atomic {
        requiredPorts   :: [(PortName,Port)],
        providedPorts   :: [(PortName,Port)],
        behavior        :: Maybe Behavior
    }    |
    Composition {
        requiredPorts   :: [(PortName,Port)],
        providedPorts   :: [(PortName,Port)],
        components      :: [(CompName,Component)],
        connections     :: [Connection]
    }
    
data Port =
    SenderReceiver [(FieldName,Type)]   |
    ClientServer [(OpName,Operation)]
    
data Operation =
    Operation {
        arguments       :: [(ArgName,Argument)],
        errors          :: [Int]
    }
    
data Argument   = 
    In    Type  |
    InOut Type  |
    Out   Type

type Connection = (PortRef, PortRef)
type PortRef    = (CompName, PortName)

data Type =
    TPrim   TypeName            |
    TStruct [(FieldName,Type)]  |
    TUnion  [(TagName,Type)]    |
    TArray  Type

type PortName   = String
type CompName   = String
type FieldName  = String
type OpName     = String
type ArgName    = String
type TagName    = String
type VarName    = String
type TypeName   = String

--------------------------------------------------------------

data Behavior =
    Behavior {
        runnables       :: [(Event,Runnable)],
        variables       :: [VarName],
        parameters      :: [VarName]
    }

data Event =
    DataReceived PortName               |
    OperationInvoked PortName OpName    |
    Timing Double                       |
    Init

data Runnable =
    Runnable {
--      ...
    }

