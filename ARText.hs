module ARText where

import Data.Map

data Package        = Package {
                            packagename     :: QualName,
                            imports         :: [Import],
                            typedefsApp     :: Map TypeName Type,
                            typedefsImpl    :: Map TypeName Type,
                            mappingSets     :: Map MapName Mapping,
                            constraints     :: Map ConstrName Constraint,
                            constants       :: Map ConstName Constant,
                            interfaces      :: Map IfaceName Interface,
                            components      :: Map CompName Component,
                            behaviors       :: Map BehName Behavior,
                            implementations :: Map ImpName Implementation,
                            modegroups      :: Map GroupName ModeGroup,
                            compositions    :: Map CompName Composition,
                            root            :: CompName
                        }

type Name           = Int
type TypeName       = Name
type MapName        = Name
type ConstrName     = Name
type ConstName      = Name
type IfaceName      = Name
type CompName       = Name
type BehName        = Name
type ImpName        = Name
type InstName       = Name
type ProtName       = Name
type GroupName      = Name
type ModeName       = Name
type PortName       = Name
type ErrName        = Name
type OpName         = Name
type ElemName       = Name
type VarName        = Name
type FieldName      = Name
type EnumName       = Name
type ParName        = Name
type ExclName       = Name
type RunName        = Name
type ShortName      = Name
type ParNameOrStar  = Name
type ElemNameOrStar = Name
type OpNameOrStar   = Name

type QualName       = [Name]

data Import         = Import QualName
                    | ImportAll QualName

-- Types --------------------------------------------------------------------------------

data Type           = TBool     InvalidValue Extends
                    | TInt      Min Max Unit ConstraintRef InvalidValue Extends
                    | TReal     Min Max Encoding AllowNaN Unit InvalidValue Extends ConstraintRef
                    | TString   Length Encoding InvalidValue
                    | TArray    TypeName Int
                    | TRecord   (Map FieldName TypeName)
                    | TEnum     Min Max (Map EnumName (Maybe Int))
                    | TFixed    Slope Bias Min Max Unit ConstraintRef InvalidValue Extends

data Min            = Min Value Interval

data Max            = Max Value Interval

data Length         = Length Int

data Interval       = Closed 
                    | Open 
                    | Infinite

data Unit           = Unit QualName
                    | NoUnit

data ConstraintRef  = ConstraintRef ConstrName
                    | NoConstraint
                    
data InvalidValue   = InvalidValue Value
                    | NoInvalid

data Extends        = Extends QualName
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
                    
data Value          = Void                      --
                    | VBool     Bool
                    | VInt      Int
                    | VReal     Double
                    | VString   String
                    | VArray    [Value]
--                    | VArray    TypeName ArrayValue
                    | VRecord   TypeName (Map FieldName Value)
                    | VEnum     EnumName
                    | VRef      ConstName
                    deriving (Eq,Ord,Show)

data ArrayValue     = Init      [Value]
                    | InitAll   Value
                    deriving (Eq,Ord,Show)

-- Interfaces ----------------------------------------------------------------------------

data Interface      = SenderReceiver    Service (Map ElemName Data)
                    | ClientServer      Service (Map ErrName Int) (Map OpName Operation)
                    | Param             Service (Map ParName Param)
                    | ModeSwitch        Service (Map ProtName GroupName)

data Service        = IsService
                    | NotService

data Data           = Data TypeName Queued InitValue

data Queued         = Queued
                    | UnQueued

data InitValue      = InitValue Value
                    | NoInitValue
                    
data Operation      = Operation [ErrName] (Map ParName Argument)

data Argument       = In        TypeName Policy
                    | InOut     TypeName Policy
                    | Out       TypeName Policy

data Policy         = UseArgumentType
                    | UseArrayBaseType
                    | UseVoid
                    | NoPolicy

data Param          = TypeName String InitValue

-- Components ---------------------------------------------------------------------------

data Component      = Application       (Map PortName Port)
                    | SensorActuator    (Map PortName Port) Hw
                    | Service           (Map PortName Port)
                    | Parameter         (Map PortName Port)

data Port           = SenderProvides    IfaceName (Map ElemName ComSpecS)
                    | ReceiverRequires  IfaceName (Map ElemName ComSpecR)
                    | ClientRequires    IfaceName (Map OpName ComSpec0)
                    | ServerProvides    IfaceName (Map OpName ComSpec1)
                    | ParamProvides     IfaceName
                    | ParamRequires     IfaceName

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

data Hw             = Hw QualName

data Behavior       = InternalBehavior {
                            supportsMultipleInstantiation   :: Bool,
                            forComponent                    :: CompName,
                            dataTypeMappings                :: [MapName],
                            exclusiveAreas                  :: [ExclName],
                            interRunnableVariables          :: Map VarName Variable,
                            calibrationParams               :: Map ParName CalParam,
                            perInstanceMemories             :: Map Name PerInstMem,
                            portAPIOptions                  :: [PortAPIOption],
                            runnables                       :: Map RunName Runnable
                        }
                            
data Variable       = Var TypeName Explicit InitValue

data Explicit       = Explicit
                    | Implicit

data CalParam       = InstanceParam     TypeName String
                    | SharedParam       TypeName String
                    
data PerInstMem     = PerInstanceMemory String String
                    
data PortAPIOption  = PortAPIOption IndirectAPI TakeAddress PortName [(Type,Value)]

data IndirectAPI    = IndirectAPI
                    | NoIndirectAPI

data TakeAddress    = EnableTakeAddress
                    | DisableTakeAddress

data Runnable       = Runnable {
                            concurrent              :: Bool,
                            minimumStartInterval    :: Double,
                            inExclusiveAreas        :: [ExclName],
                            usesExclusiveAreas      :: [ExclName],
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

data Event          = DataReceivedEvent PortName ElemName As Dis
                    | OperationInvokedEvent PortName OpName As Dis
                    | ModeSwitchEvent Activation PortName GroupName ModeName As Dis
                    
                    | InitEvent
                    | BackgroundEvent
                    | TimingEvent Double As Dis

                    | DataSendCompletedEvent ShortName As Dis
                    | DataWriteCompletedEvent PortName ElemName
                    | AsynchronousServerCallReturnsEvent ServerCallPt
                    | ModeSwitchAckEvent PortName GroupName

                    | ReceiveErrorEvent PortName ElemName As Dis
                    | ModeManagerErrorEvent

                    | ExternalTriggerOccurredEvent
                    | InternalTriggerOccurredEvent
{-
data WPEvent        = DataSendCompleted                 -- Rte_Feedback(PortName,ElemName)
                    | DataReceived                      -- Rte_Receive(PortName,ElemName,...)
                    | AsynchronousServerCallReturns     -- Rte_Result(PortName,OpName,...)
                    | ModeSwitchAck                     -- Rte_SwitchAck(PortName,GroupName)
                    
-}
                    
data ParamAccess    = ParameterAccess ParName As
                    | ParamPortAccess PortName ParNameOrStar As

data DataRdAccess   = DataReadAccess PortName ElemNameOrStar As

data DataRcvPt      = DataReceivePoint PortName ElemNameOrStar As

data DataSndPt      = DataSendPoint PortName ElemNameOrStar As

data DataWrAccess   = DataWriteAccess PortName ElemNameOrStar As

data ModeSwitchPt   = ModeSwitchPoint PortName ProtName As

data ModeAccessPt   = ModeAccessPoint Activation PortName GroupName As

data ServerCallPt   = ServerCallPoint SyncOrAsync TimeOut PortName OpNameOrStar As

data WaitPt         = WaitPoint ShortName TimeOut [ShortName]

data SyncOrAsync    = Synchronous
                    | Asynchronous

data Activation     = Entry
                    | Exit

data As             = As ShortName
                    | NoName

data Dis            = DisabledFor PortName GroupName ModeName
                    | NoDis
                    
data Implementation = Implementation {
                            forBehavior         :: BehName,
                            language            :: Language,
                            codeDescriptor      :: String,
                            codeGenerator       :: Maybe String,
                            requiredRTEVendor   :: RTEVendor,
                            compilers           :: [Compiler]
                        }

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

data ModeGroup      = ModeGroup Initial [ModeName]

data Initial        = Initial ModeName
                    | NoInitial

-- Compositions ----------------------------------------------------------------------------

data Composition    = Composition {
                            subcomponents   :: Map InstName CompPrototype,
                            delegations     :: Map PortName Delegation,
                            connectors      :: [Connector]
                        }
                        
data CompPrototype  = Prototype CompName

data Delegation     = DelegateRequires  IfaceName [(InstName,PortName)]
                    | DelegateProvides  IfaceName [(InstName,PortName)]
                    deriving (Eq)

data Connector      = Connect           (InstName,PortName) (InstName,PortName)
                    | AutoConnect       InstName InstName
                    deriving (Eq)

