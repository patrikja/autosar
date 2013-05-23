module AR where


data System = 
    System {
        root            :: Component,
        functions       :: [(FunName,Function)]
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
    SenderReceiver [(VarName,Type)]   |
    ClientServer [(OpName,Operation)]
    
data Operation =
    Operation {
        arguments       :: [(VarName,Argument)],
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
type TagName    = String
type VarName    = String
type TypeName   = String
type FunName    = String

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
        args            :: [(VarName,Argument)],
        implementation  :: [Stmt]
    }

data Stmt =
    SBind Type VarName Expr     |
    SAssign VarName Expr        |
    SSend PortName [Expr]       |
    SInvoke VarName PortName OpName [Expr]   |
    SIf Expr [Stmt] [Stmt]      |
    SWhile Expr [Stmt]
    
data Function =
    Function {
        params          :: [(VarName,Type)],
        result          :: Type,
        body            :: Expr
    }
    
data Expr =
    EVar VarName                |
    ECall FunName [Expr]        |
    EArray [Expr]               |
    EIndex Expr Expr            |
    EStruct [(FieldName,Expr)]  |
    ESelect Expr FieldName      |
    EInt Int                    |
    EChar Char                  |
    EIf Expr Expr Expr          |
    ELet Type VarName Expr Expr
