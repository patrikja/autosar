module ARText where

import Data.Map

data Package        = Package {
                            packagename     :: [Name],
                            imports         :: [Import],
                            typedefsApp     :: Map TypeName Type,
                            typedefsImpl    :: Map TypeName Type,
                            mappingSets     :: Map MapName Mapping,
                            constraints     :: Map ConstrName Constraint,
                            constants       :: Map ConstName Constant,
                            interfaces      :: Map IfaceName Interface,
                            components      :: Map CompName Component,
                            modegroups      :: Map GroupName ModeGroup
                        }

type Name           = String
type TypeName       = String
type MapName        = String
type ConstrName     = String
type ConstName      = String
type IfaceName      = String
type CompName       = String
type GroupName      = String
type PortName       = String
type ErrName        = String
type OpName         = String
type ElemName       = String
type VarName        = String

data Import         = Import [Name] Name
                    | ImportAll [Name]

-- Types --------------------------------------------------------------------------------

data Type           = TBool     InvalidValue Extends
                    | TInt      Min Max Unit ConstraintRef InvalidValue Extends
                    | TReal     Min Max Encoding AllowNaN Unit InvalidValue Extends ConstraintRef
                    | TString   Length Encoding InvalidValue
                    | TArray    TypeName Int
                    | TRecord   (Map Name TypeName)
                    | TEnum     Min Max (Map Name (Maybe Int))
                    | TFixed    Slope Bias Min Max Unit ConstraintRef InvalidValue Extends

data Min            = Min Value Interval

data Max            = Max Value Interval

data Length         = Length Int

data Interval       = Closed 
                    | Open 
                    | Infinite

data Unit           = Unit [Name]
                    | NoUnit

data ConstraintRef  = ConstraintRef ConstrName
                    | NoConstraint
                    
data InvalidValue   = InvalidValue Value
                    | NoInvalid

data Extends        = Extends [Name]
                    | NoExtends

data Encoding       = EncodingDouble
                    | EncodingSingle
                    | Encoding String
                    | NoEncoding

data AllowNaN       = AllowNaN
                    | NoAllowNaN

data Slope          = Slope Double

data Bias           = Bias Double
                    | NoBias

-- Mappings -------------------------------------------------------------------------------

data Mapping        = MapT (Map TypeName TypeName)
                    | MapG (Map TypeName GroupName)

-- Constraints ----------------------------------------------------------------------------

data Constraint     = Constraint [Rule]

data Rule           = Rule PhysInt Min Max Unit

data PhysInt        = Physical
                    | Internal

-- Constants ------------------------------------------------------------------------------

data Constant       = Const     TypeName Value
                    
data Value          = VBool     Bool
                    | VInt      Int
                    | VReal     Double
                    | VString   String
                    | VArray    TypeName ArrayValue
                    | VRecord   TypeName (Map Name Value)
                    | VEnum     Name
                    | VRef      ConstName

data ArrayValue     = Init      [Value]
                    | InitAll   Value

-- Interfaces ----------------------------------------------------------------------------

data Interface      = SenderReceiver    Service (Map ElemName Data)
                    | ClientServer      Service (Map ErrName Int) (Map OpName Operation)
                    | Param             Service (Map Name Param)
                    | ModeSwitch        Service (Map Name GroupName)

data Service        = IsService
                    | NotService

data Data           = Data TypeName Queued InitValue

data Queued         = Queued
                    | UnQueued

data InitValue      = InitValue Value
                    | NoInitValue
                    
data Operation      = Operation [ErrName] (Map Name Argument)

data Argument       = In        TypeName Policy
                    | InOut     TypeName Policy
                    | Out       TypeName Policy

data Policy         = UseArgumentType
                    | UseArrayBaseType
                    | UseVoid
                    | NoPolicy

data Param          = TypeName String InitValue

-- Components ---------------------------------------------------------------------------

data Component      = Application       (Map PortName Port) Behavior Implementation
                    | SensorActuator    (Map PortName Port) Behavior Implementation Hw
                    | Service           (Map PortName Port)
                    | Parameter         (Map PortName Port)
                    | Composition       (Map PortName Port) (Map Name CompPrototype) [Connector]

data Port           = SenderProvides    IfaceName (Map ElemName ComSpecS)
                    | ReceiverRequires  IfaceName (Map ElemName ComSpecR)
                    | ClientRequires    IfaceName (Map OpName ComSpec0)
                    | ServerProvides    IfaceName (Map OpName ComSpec1)
                    | ParamProvides     IfaceName
                    | ParamRequires     IfaceName [(Name,PortName)]
                    | DelegateProvides  IfaceName [(Name,PortName)]

data CompPrototype  = Prototype CompName

data Connector      = Connect           Name PortName Name PortName
                    | AutoConnect       Name Name

data ComSpecS       = QueuedComSpecS    CanInvalidate InitValue E2EProtection OutOfRange
                    | UnQueuedComSpecS  E2EProtection OutOfRange

data ComSpecR       = QueuedComSpecR    Length E2EProtection OutOfRange
                    | UnQueuedComSpecR  TimeOut ResyncTime InvalidType InitValue
                                        EnableUpdate NeverReceived E2EProtection OutOfRange

data ComSpec0       = ComSpec0

data ComSpec1       = ComSpec1 Length

data CanInvalidate  = CanInvalidate
                    | CannotInvalidate
                    
data E2EProtection  = UsesEndToEndProtection
                    | NoEndToEndProtection

data OutOfRange     = NONE
                    | IGNORE
                    | SATURATE
                    | DEFAULT
                    | INVALID

data TimeOut        = TimeOut Double
                    | NoTimeOut

data ResyncTime     = ResyncTime Double
                    | NoResyncTime

data InvalidType    = HandleInvalidTypeKeep
                    | HandleInvalidTypeReplace
                    | NoHandleInvalidType

data EnableUpdate   = EnableUpdate Bool
                    | NoEnableUpdate

data NeverReceived  = HandleNeverReceived
                    | NoHandleNeverReceived

data Hw             = Hw [Name]

data Behavior       = InternalBehavior {
                            supportsMultipleInstantiation   :: Bool,
                            behaviorName                    :: Name,
                            forComponent                    :: CompName,
                            dataTypeMappings                :: [MapName],
                            exclusiveAreas                  :: [Name],
                            interRunnableVariables          :: Map VarName Variable,
                            calibrationParams               :: Map Name CalParam,
                            perInstanceMemories             :: Map Name PerInstMem,
                            portAPIOptions                  :: [PortAPIOption],
                            runnables                       :: Map Name Runnable
                        }
                    | NoInternalBehavior
                            
data Variable       = Var TypeName Explicit InitValue

data Explicit       = Explicit
                    | Implicit

data CalParam       = InstanceParam     TypeName String
                    | SharedParam       TypeName String
                    
data PerInstMem     = PerInstanceMemory String String
                    
data PortAPIOption  = PortAPIOption IndirectAPI TakeAddress 

data IndirectAPI    = IndirectAPI
                    | NoIndirectAPI

data TakeAddress    = EnableTakeAddress
                    | DisableTakeAddress

data Runnable       = Runnable {
                            concurrent              :: Bool,
                            minimumStartInterval    :: Double,
                            inExclusiveAreas        :: [Name],
                            usesExclusiveAreas      :: [Name],
                            symbol                  :: Maybe String,
                            readVariables           :: [VarName],
                            writtenVariables        :: [VarName],
                            events                  :: [Event],
                            parameterAccesses       :: [ParamAccess],
                            dataReadAccesses        :: [DataRdAccess],
                            dataReceivePoints       :: [DataRcvPt],
                            dataSendPoints          :: [DataSndPt],
                            dataWriteAccesses       :: [DataWrAccess],
                            modeSwitchPoints        :: [ModeSwitchPt],
                            modeAccessPoints        :: [ModeAccessPt],
                            serverCallPoints        :: [ServerCallPt],
                            waitPoints              :: [WaitPt]
                        }

data Event          = DataSendCompletedEvent Name As Dis
                    | DataReceivedEvent PortName ElemName As Dis
                    | ReceiveErrorEvent PortName ElemName As Dis
                    | TimingEvent Double As Dis
                    | ModeSwitchEvent Activation PortName GroupName Name As Dis
                    | OperationInvokedEvent PortName OpName As Dis
                    
data ParamAccess    = ParameterAccess Name As
                    | ParamPortAccess PortName Name As

data DataRdAccess   = DataReadAccess PortName ElemName As

data DataRcvPt      = DataReceivePoint PortName ElemName 

data DataSndPt      = DataSendPoint PortName ElemName As

data DataWrAccess   = DataWriteAccess PortName ElemName As

data ModeSwitchPt   = ModeSwitchPoint PortName Name As

data ModeAccessPt   = ModeAccessPoint Activation PortName GroupName As

data ServerCallPt   = ServerCallPoint SyncOrAsync TimeOut PortName OpName As

data WaitPt         = WaitPoint Name TimeOut [Name]

data SyncOrAsync    = Synchronous
                    | Asynchronous

data Activation     = Entry
                    | Exit

data As             = As Name
                    | NoName

data Dis            = DisabledFor PortName GroupName Name
                    | NoDis
                    
data Implementation = Implementation {
                            implName            :: Name,
                            forBehavior         :: Name,
                            language            :: Language,
                            codeDescriptor      :: String,
                            codeGenerator       :: Maybe String,
                            requiredRTEVendor   :: RTEVendor,
                            compilers           :: [Compiler]
                        }
                    | NoImplementation

data Language       = C
                    | Cpp
                    | Java

data RTEVendor      = RTEVendor String SwVersion VendorId
                    | NoRTEVendor
                    
data SwVersion      = SwVersion Int
                    | NoSwVersion

data VendorId       = VendorId Int
                    | NoVendorId
                    
data Compiler       = Compiler {
                            compilerName        :: Name,
                            vendor              :: String,
                            version             :: String
                        }
                        
-- ModeGroups ------------------------------------------------------------------------------

data ModeGroup      = ModeGroup Initial [Name]

data Initial        = Initial Name
                    | NoInitial

