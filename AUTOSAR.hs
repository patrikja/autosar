module AUTOSAR where

class Referable a where
        shortName                   :: a -> String

-----------------------------------------------------------------
-- System
-----------------------------------------------------------------

data System = System {
        ecuExtractVersion               :: Maybe String,
        fibexElement                    :: [FibexElement],      -- ref
        mapping_1                       :: [SystemMapping],
        pncVectorLength                 :: Maybe Int,
        pncVectorOffset                 :: Maybe Int,
        rootSoftwareComposition         :: Maybe RootSwCompositionPrototype,
        systemDocumentation             :: [String],
        systemVersion                   :: String
    }

data RootSwCompositionPrototype = RootSwCompositionPrototype {
        calibrationParameterValueSet    :: [CalibrationParameterValueSet],  -- ref
        flatMap                         :: Maybe FlatMap,                   -- ref
        softwareComposition             :: SwComponentType                  -- tref
    }

-----------------------------------------------------------------
-- SystemMapping
-----------------------------------------------------------------

data SystemMapping = SystemMapping {
        dataMapping                     :: [DataMapping],
        ecuResourceMapping              :: [EcuMapping],
        mappingConstraint               :: [MappingConstraint],
        pncMapping                      :: [PncMapping],
        resourceEstimation              :: [EcuResourceEstimation],
        signalPathConstraint            :: [SignalPathConstraint],
        swImplMapping                   :: [SwcToImplMapping],
        swMapping                       :: [SwcToEcuMapping]
    }

data MappingConstraint =
    ComponentClustering {
        introduction                    :: Maybe String,
        clusteredComponent              :: [SwComponentPrototype]
    } |
    ComponentSeparation {
        introduction                    :: Maybe String,
        separatedComponent              :: [SwComponentPrototype]
    } |
    SwcToEcuMappingConstraint {
        introduction                    :: Maybe String,
        component_4                     :: SwComponentPrototype,
        ecuInstance_1                   :: [EcuInstance],               -- ref
        swcToEcuMappingConstraintType   :: SwcToEcuMappingConstraintType
    }

data SwcToEcuMappingConstraintType      = Dedicated | Exclusive

data SwcToImplMapping = SwcToImplMapping {
        component_1                     :: [SwComponentPrototype],      -- iref
        componentImplementation         :: SwcImplementation            -- ref
    }
    
data SwcToEcuMapping = SwcToEcuMapping {
        component_3                     :: [SwComponentPrototype],      -- iref
        controlledHwElement             :: Maybe HwElement,             -- ref
        ecuInstance_2                   :: EcuInstance,                 -- ref
        partition                       :: Maybe EcuPartition,          -- ref
        processingUnit                  :: Maybe HwElement              -- ref
    }

-----------------------------------------------------------------
-- SwComponent
-----------------------------------------------------------------

data SwComponentType_ = SwComponentType {
        shortName_1                 :: String,
        consistencyNeeds            :: [ConsistencyNeeds],
        port                        :: [PortPrototype],
        portGroup                   :: [PortGroup],
        swComponentDocumentation    :: (Maybe SwComponentDocumentation),
        unitGroup                   :: [UnitGroup]                          -- ref
    }

instance Referable SwComponentType_ where
        shortName                   = shortName_1
    
data SwComponentType =
    AtomicSwComponent { 
        swComponentType             :: SwComponentType_,
        internalBehavior            :: Maybe SwcInternalBehavior,
        symbolProps                 :: Maybe SymbolProps
    } |
    ParameterSwComponentType {
        swComponentType             :: SwComponentType_,
        constantMapping             :: [ConstantSpecificationMappingSet],
        dataTypeMapping_1           :: [DataTypeMappingSet],
        instantiationDataDefProps_1 :: [InstantiationDataDefProps]
    } |
    CompositionSwComponentType {
        swComponentType             :: SwComponentType_,
        component_2                 :: [SwComponentPrototype],
        connector                   :: [SwConnector],
        constantValueMapping_2      :: [ConstantSpecificationMappingSet],   -- ref
        dataTypeMapping_2           :: [DataTypeMappingSet],                -- ref
        instantiationRTEEventProps  :: [InstantiationRTEEventProps]
    }
    
instance Referable SwComponentType where
        shortName                   = shortName . swComponentType

data SwComponentPrototype = SwComponentPrototype {
        shortName_3                 :: String,
        type_1                      :: SwComponentType                  -- tref
    }

-----------------------------------------------------------------
-- PortPrototype
-----------------------------------------------------------------

data PortPrototype_ = PortPrototype {
        shortName_2                     :: String,
        clientServerAnnotation          :: [ClientServerAnnotation],
        delegatedPortAnnotation         :: Maybe DelegatedPortAnnotation,
        ioHwAbstractionServerAnnotation :: [IoHwAbstractionServerAnnotation],
        modePortAnnotation              :: [ModePortAnnotation],
        nvDataPortAnnotation            :: [NvDataPortAnnotation],
        parameterPortAnnotation         :: [ParameterPortAnnotation],
        senderReceiverAnnotation        :: [SenderReceiverAnnotation],
        triggerPortAnnotation           :: [TriggerPortAnnotation]
    }

data PortPrototype =
    RPortPrototype {
        portPrototype               :: PortPrototype_,
        requiredComSpec             :: [RPortComSpec],
        requiredInterface           :: PortInterface                -- tref
    } |
    PPortPrototype {
        portPrototype               :: PortPrototype_,
        providedComSpec             :: [PPortComSpec],
        providedInterface           :: PortInterface                -- tref
    } |
    PRPortPrototype {
        portPrototype               :: PortPrototype_,
        requiredComSpec             :: [RPortComSpec],
        requiredInterface           :: PortInterface,
        providedComSpec             :: [PPortComSpec],
        providedInterface           :: PortInterface
    }


-----------------------------------------------------------------
-- PortInterface
-----------------------------------------------------------------

data PortInterface_ = PortInterface {
        shortName_4                 :: String,
        isService                   :: Bool,
        serviceKind                 :: Maybe ServiceProviderEnum
    }

data PortInterface =
    ParameterInterface {
        portInterface               :: PortInterface_,
        parameter                   :: [ParameterDataPrototype]
    } |
    SenderReceiverInterface {
        portInterface               :: PortInterface_,
        dataElement                 :: [VariableDataPrototype],
        invalidationPolicy          :: [InvalidationPolicy]
    } |
    NvDataInterface {
        portInterface               :: PortInterface_,
        nvData                      :: [VariableDataPrototype]
    } |
    ModeSwitchInterface {
        portInterface               :: PortInterface_,
        modeGroup                   :: ModeDeclarationGroupPrototype
    } |
    TriggerInterface {
        portInterface               :: PortInterface_,
        trigger_1                   :: [Trigger]
    } |
    ClientServerInterface {
        portInterface               :: PortInterface_,
        operation_1                 :: [ClientServerOperation],
        possibleError_1             :: [ApplicationError]
    }


-----------------------------------------------------------------
-- SwConnector
-----------------------------------------------------------------

data SwConnector =
    AssemblySwConnector {
        mapping_2                   :: Maybe PortInterfaceMapping,      -- ref
        provider                    :: PortPrototype,                   -- iref
        requester                   :: PortPrototype                    -- iref
    } |
    DelegationSwConnector {
        mapping_2                   :: Maybe PortInterfaceMapping,      -- ref
        innerPort                   :: PortPrototype,                   -- iref
        outerPort                   :: PortPrototype                    -- ref
    } |
    PassThroughSwConnector {
        mapping_2                   :: Maybe PortInterfaceMapping,      -- ref
        providedOuterPort           :: PortPrototype,                   -- ref
        requiredOuterPort           :: PortPrototype                    -- ref
    }


-----------------------------------------------------------------
-- SwInternalBehavior
-----------------------------------------------------------------

data SwcInternalBehavior = SwcInternalBehavior {
        shortName_12                    :: String,
        constantMemory                  :: [ParameterDataPrototype],
        constantValueMapping_1          :: [ConstantSpecificationMappingSet],
        dataTypeMapping                 :: [DataTypeMappingSet],
        exclusiveArea                   :: [ExclusiveArea],
        exclusiveAreaNestingOrder_1     :: [ExclusiveAreaNestingOrder],
        staticMemory                    :: [VariableDataPrototype],
        arTypedPerInstanceMemory        :: [VariableDataPrototype],
        event                           :: [RTEEvent],
        explicitInterRunnableVariable   :: [VariableDataPrototype],
        handleTerminationAndRestart     :: HandleTerminationAndRestart,
        implicitInterRunnableVariable   :: [VariableDataPrototype],
        includedDataTypeSet             :: [IncludedataTypeSet],
        includedModeDeclarationGroupSet :: [IncludedModeDeclarationGroupSet],
        instantiationDataDefProps_2     :: [InstantiationDataDefProps],
        perInstanceMemory_1             :: [PerInstanceMemory],
        perInstanceParameter            :: [ParameterDataPrototype],
        portAPIOption                   :: [PortAPIOption],
        runnable                        :: [RunnableEntity],
        serviceDependency               :: [ServiceDependency],
        sharedParameter                 :: [ParameterDataPrototype],
        supportsMultipleInstantiation   :: Bool,
        variationPointProxy             :: [VariationPointProxy]
    }

data ParameterDataPrototype = ParameterDataPrototype {
        shortName_7                 :: String,
        type_2                      :: AutosarDataType,
        initValue_1                 :: Maybe ValueSpecification
    }

data VariableDataPrototype = VariableDataPrototype {
        shortName_8                 :: String,
        type_3                      :: AutosarDataType,
        initValue_2                 :: Maybe ValueSpecification
    }

data ArgumentDataPrototype = ArgumentDataPrototype {
        shortName_10                :: String,
        type_4                      :: AutosarDataType,
        direction                   :: ArgumentDirectionEnum,
        serverArgumentImplPolicy    :: Maybe ServerArgumentImplPolicy,
        typeBlueprint               :: AutosarDataType
    }

data ArgumentDirectionEnum          = In | InOut | Out

data VariableAccess = VariableAccess {
        accessedVariable            :: AutosarVariableRef,
        scope                       :: Maybe VariableAccessScopeEnum  -- for selected roles only
    }

data AutosarVariableRef =
        AutosarVariable                 DataPrototype |
        AutosarVariableInImplDatatype   ArVariableInImplementationDataInstanceRef |
        LocalVariable                   VariableDataPrototype
        

data VariableAccessScopeEnum = 
        CommunicationInterEcu |
        CommunicationInterPartition |
        InterPartitionIntraEcu
        
data Trigger = Trigger {
        shortName_5                 :: String,
        swImplPolicy                :: Maybe SwImplPolicyEnum,
        triggerPeriod               :: Maybe MultidimensionalTime
    }

data ClientServerOperation = ClientServerOperation {
        shortName_9                 :: String,
        argument_1                  :: [ArgumentDataPrototype],
        possibleError_2             :: [ApplicationError]
    }

data AutosarDataType = AutosarDataType

data ApplicationError = ApplicationError {
        shortName_11                :: String,
        errorCode                   :: Int
    }

data HandleTerminationAndRestart =
    CanBeTerminated |
    CanBeTerminatedAndRestarted |
    NoSupport


-----------------------------------------------------------------
-- RTEEvent
-----------------------------------------------------------------

data RTEEvent_ = RTEEvent {
        disabledMode                :: [ModeDeclaration],       -- iref
        startOnEvent                :: Maybe RunnableEntity     -- ref
    }

data RTEEvent =
    AsynchronousServerCallReturnsEvent {
        rteEvent                    :: RTEEvent_,
        eventSource_1               :: AsynchronousServerCallResultPoint    -- ref
    } |
    DataSendCompletedEvent {
        rteEvent                    :: RTEEvent_,
        eventSource_2               :: VariableAccess                       -- ref
    } |
    DataWriteCompletedEvent {
        rteEvent                    :: RTEEvent_,
        eventSource_3               :: VariableAccess                       -- ref
    } |
    DataReceivedEvent {
        rteEvent                    :: RTEEvent_,
        data_1                      :: Maybe VariableDataPrototype          -- iref
    } |
    DataReceiveErrorEvent {
        rteEvent                    :: RTEEvent_,
        data_2                      :: Maybe VariableDataPrototype          -- iref
    } |
    OperationInvokedEvent {
        rteEvent                    :: RTEEvent_,
        operation_2                 :: Maybe ClientServerOperation          -- iref
    } |
    TimingEvent {
        rteEvent                    :: RTEEvent_,
        period                      :: TimeValue
    } |
    BackGroundEvent {
        rteEvent                    :: RTEEvent_
    } |
    SwcModeSwitchEvent {
        rteEvent                    :: RTEEvent_,
        activation                  :: ModeActivationKind,
        mode                        :: (ModeDeclaration, Maybe ModeDeclaration)
    } |
    ModeSwitchAckEvent {
        rteEvent                    :: RTEEvent_,
        eventSource_1               :: ModeSwitchPoint
    } |
    ExternalTriggerOccurredEvent {
        rteEvent                    :: RTEEvent_,
        trigger_2                   :: Maybe Trigger
    } |
    InternalTriggerOccurredEvent {
        rteEvent                    :: RTEEvent_,
        eventSource_4               :: InternalTriggeringPoint
    } |
    InitEvent {
        rteEvent                    :: RTEEvent_
    }

data ModeActivationKind             = OnEntry | OnExit | OnTransition

-----------------------------------------------------------------
-- Runnable
-----------------------------------------------------------------

data ExecutableEntity = ExecutableEntity {
        activationReason                    :: [ExecutableEntityActivationReason],
        canEnterExclusiveArea               :: [ExclusiveArea],
        exclusiveAreaNestingOrder_2         :: [ExclusiveAreaNestingOrder],
        minimumStartInterval                :: TimeValue,
        reentrancyLevel                     :: Maybe ReentrancyLevelEnum,   -- only for BSW
        runsInsideExclusiveArea             :: [ExclusiveArea],
        swAddrMethod                        :: Maybe SwAddrMethod
    }

data RunnableEntity = RunnableEntity {
        executableEntity                    :: ExecutableEntity,
        argument_2                          :: [RunnableEntityArgument],    -- client/server only
        asynchronousServerCallResultPoint   :: [AsynchronousServerCallResultPoint],
        canBeInvokedConcurrently            :: Bool,
        dataReadAccess                      :: [VariableAccess],    -- cat 1 only
        dataReceivePointByArgument          :: [VariableAccess],    -- send/rcv only
        dataReceivePointByValue             :: [VariableAccess],    -- send/rcv only
        dataSendPoint                       :: [VariableAccess],    -- send/rcv only
        dataWriteAccess                     :: [VariableAccess],    -- cat 1 only
        externalTriggeringPoint             :: [ExternalTriggeringPoint],
        internalTriggeringPoint             :: [InternalTriggeringPoint],
        modeAccessPoint                     :: [ModeAccessPoint],
        modeSwitchPoint                     :: [ModeSwitchPoint],
        parameterAccess                     :: [ParameterAccess],
        readLocalVariable                   :: [VariableAccess],
        serverCallPoint                     :: [ServerCallPoint],
        symbol                              :: CIdentifier,
        waitPoint                           :: [WaitPoint],         -- not null <=> cat 2
        writtenLocalVariable                :: [VariableAccess]
    }


-----------------------------------------------------------------
-- SwImplementation
-----------------------------------------------------------------

data SwcImplementation = SwcImplementation {
        shortName_13                        :: String,
        implementation                      :: Implementation,
        behavior                            :: SwcInternalBehavior,
        perInstanceMemorySize               :: [PerInstanceMemorySize],
        requiredRTEVendor                   :: Maybe String
    }


data Implementation = Implementation {
        buildActionManifest                 :: Maybe String,
        codeDescriptor                      :: [String],
        compiler                            :: [String],
        generatedArtefact                   :: [String],
        hwElement                           :: [HwElement],             -- ref
        linker                              :: [String],
        programmingLanguage                 :: ProgrammingLanguageEnum,
        requiredArtefact                    :: [String],
        requiredGeneratorTool               :: [String],
        resourceConsumption                 :: ResourceConsumption,
        swVersion                           :: String,
        swcBswMapping                       :: Maybe SwcBswMapping,     -- ref
        usedCodeGenerator                   :: Maybe String,
        vandorId                            :: Int
    }

data PerInstanceMemory = PerInstanceMemory {
        initValue                           :: Maybe String,
        swDataDefProps                      :: SwDataDefProps,
        type_5                              :: String,
        typeDefinition                      :: String
    }

data PerInstanceMemorySize = PerInstanceMemorySize {
        alignment                           :: Integer,
        perInstanceMemory_2                 :: PerInstanceMemory,
        size                                :: Integer
    }


-----------------------------------------------------------------
-- Misc.
-----------------------------------------------------------------

type TimeValue = Double
type CIdentifier = String
type ExecutableEntityActivationReason = Int
type RunnableEntityArgument = CIdentifier


type ConsistencyNeeds = ()
type PortGroup = ()
type SwComponentDocumentation = ()
type UnitGroup = ()
type SymbolProps = ()
type ConstantSpecificationMappingSet = ()
type DataTypeMappingSet = ()
type InstantiationDataDefProps = ()
type InstantiationRTEEventProps = ()
type ClientServerAnnotation = ()
type DelegatedPortAnnotation = ()
type IoHwAbstractionServerAnnotation = ()
type ModePortAnnotation = ()
type NvDataPortAnnotation = ()
type ParameterPortAnnotation = ()
type SenderReceiverAnnotation = ()
type TriggerPortAnnotation = ()
type RPortComSpec = ()
type PPortComSpec = ()
type ServiceProviderEnum = ()
type InvalidationPolicy = ()
type ModeDeclarationGroupPrototype = ()
type SwImplPolicyEnum = ()
type MultidimensionalTime = ()
type ValueSpecification = ()
type ServerArgumentImplPolicy = ()
type PortInterfaceMapping = ()
type VariationPointProxy = ()
type ServiceDependency = ()
type PortAPIOption = ()
type IncludedModeDeclarationGroupSet = ()
type IncludedataTypeSet = ()
type ModeDeclaration = ()
type AsynchronousServerCallResultPoint = ()
type WaitPoint = ()
type ServerCallPoint = ()
type ParameterAccess = ()
type ModeSwitchPoint = ()
type ModeAccessPoint = ()
type InternalTriggeringPoint = ()
type ExternalTriggeringPoint = ()
type SwcBswMapping = ()
type ResourceConsumption = ()
type ProgrammingLanguageEnum = ()
type HwElement = ()
type ExclusiveAreaNestingOrder = ()
type ExclusiveArea = ()
type SwDataDefProps = ()
type FlatMap = ()
type CalibrationParameterValueSet = ()
type EcuPartition = ()
type EcuInstance = ()
type SignalPathConstraint = ()
type EcuResourceEstimation = ()
type PncMapping = ()
type EcuMapping = ()
type DataMapping = ()
type FibexElement = ()
type SwAddrMethod = ()
type ReentrancyLevelEnum = ()
type ArVariableInImplementationDataInstanceRef = ()
type DataPrototype = ()
