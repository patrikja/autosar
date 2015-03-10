{-# LANGUAGE OverloadedStrings #-}
module ARXML where

import Prelude hiding (elem, FilePath)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import qualified Text.XML as XML
import AUTOSAR
import qualified Data.Text as Text

import qualified System.FilePath.Posix as Posix
import qualified Filesystem.Path.CurrentOS as OS
-- import System.IO.Unsafe (unsafePerformIO)

readARXML :: String -> IO [Package]
readARXML f =
  fmap docToPackages (XML.readFile settings (OS.decodeString f))
  where settings = XML.def -- default

readAllARXML files =
  do
    ps <- mapM readARXML files
    return (List.concat ps)


docToPackages d = List.map toPackage $ elems "AR-PACKAGE" $ elem "AR-PACKAGES" $ XML.documentRoot d

ns s = XML.Name { XML.nameLocalName = s, XML.nameNamespace = Just "http://autosar.org/schema/r4.0", XML.namePrefix = Nothing }

isElem s e = (XML.elementName e) == (ns s)

allElems e =
  (List.concatMap f (XML.elementNodes e))
  where f (XML.NodeElement e) = [e]
        f _ = []

firstElem e =
  List.head $ allElems e

elems :: XML.Name -> XML.Element -> [XML.Element]
elems name e = -- could use allElems
  (List.concatMap f) $ XML.elementNodes e
  -- ignore namespaces here:
  where f (XML.NodeElement e) = if (XML.nameLocalName name) == (XML.nameLocalName (XML.elementName e)) then [e] else []
        f _ = []

optElem name e = listToMaybe $ elems name e

name_unpack n = Text.unpack (XML.nameLocalName n)

-- elem :: XML.Name -> XML.Element -> XML.Element
elem name e =
  get_ $ optElem name e
  where get_ (Just x) = x
        get_ Nothing = error ("No child element '" ++ (name_unpack name) ++ "' found in '" ++ (name_unpack $ XML.elementName e) ++ "'.")

text e =
  let [XML.NodeContent c] = XML.elementNodes e -- or merge all?
  in Text.unpack c

optChildElems name e =
  maybe [] childElems $ optElem name e
  where childElems x = (List.concatMap f) $ XML.elementNodes x
        f (XML.NodeElement e) = [e]
        f _ = []

optAttr name e = (Map.lookup name) $ XML.elementAttributes e

attr name e = fromJust $ optAttr name e

-- common attributes ---

_shortName e =
  text $ elem "SHORT-NAME" e

optVariationPoint e =
  (fmap toVariationPoint) $ optElem "VARIATION-POINT" e

-- convert an absolute (!) path expression /abc/def to a list of strings ["abc",
-- "def"].
-- Not actually defined as posix paths, but is will be sufficient.
aref e =
  AR dest path
  where path = List.drop 1 $ Posix.splitDirectories (text e) -- first will
           -- always be "/"
        dest = toDest $ Text.unpack $ (attr "DEST" e)

typeRef e = aref $ elem "TYPE-TREF" e

iref contextNames targetName e =
  IAR context target
  where context = getOrderedContext contextNames e
        target = aref $ List.head (List.filter (\e -> isElem targetName e) children)
        children = allElems e

getOrderedContext [] e = []
getOrderedContext (cname:contextNames) e =
  -- (little bit stupid impl)
  (List.map (\e -> CAR $ aref e) (List.filter (\e -> List.any (\t -> isElem t e) [cname]) (allElems e))) ++ (getOrderedContext contextNames e)

toDest "SW-SYSTEMCONST" = RSESwSystemconst
toDest "SENDER-RECEIVER-INTERFACE" = RSESenderReceiverInterface
toDest "DATA-TYPE-MAPPING-SET" = RSEDataTypeMappingSet
toDest "RUNNABLE-ENTITY" = RSERunnableEntity
toDest "R-PORT-PROTOTYPE" = RSERPortPrototype
toDest "VARIABLE-DATA-PROTOTYPE" = RSEVariableDataPrototype
toDest "P-PORT-PROTOTYPE" = RSEPPortPrototype
toDest "TRIGGER" = RSETrigger
toDest "SWC-INTERNAL-BEHAVIOR" = RSESwcInternalBehavior
toDest "APPLICATION-PRIMITIVE-DATA-TYPE" = RSEApplicationPrimitiveDataType
toDest "MODE-SWITCH-INTERFACE" = RSEModeSwitchInterface
toDest "CLIENT-SERVER-INTERFACE" = RSEClientServerInterface
toDest "PARAMETER-INTERFACE" = RSEParameterInterface
toDest "TRIGGER-INTERFACE" = RSETriggerInterface
toDest "NV-DATA-INTERFACE" = RSENvDataInterface
toDest "APPLICATION-ARRAY-DATA-TYPE" = RSEApplicationArrayDataType
toDest "APPLICATION-SW-COMPONENT-TYPE" = RSEApplicationSwComponentType
toDest "SENSOR-ACTUATOR-SW-COMPONENT-TYPE" = RSESensorActuatorSwComponentType
toDest "COMPOSITION-SW-COMPONENT-TYPE" = RSECompositionSwComponentType
toDest "SW-COMPONENT-PROTOTYPE" = RSESwComponentPrototype
toDest "VARIABLE-AND-PARAMETER-INTERFACE-MAPPING" = RSEVariableAndParameterInterfaceMapping
toDest "PARAMETER-SW-COMPONENT-TYPE" = RSEParameterSwComponentType
toDest "NV-BLOCK-SW-COMPONENT-TYPE" = RSENvBlockSwComponentType
toDest "FLAT-MAP" = RSEFlatMap
toDest "PR-PORT-PROTOTYPE" = RSEPrPortPrototype
toDest "MODE-DECLARATION-GROUP" = RSEModeDeclarationGroup
toDest "IMPLEMENTATION-DATA-TYPE" = RSEImplementationDataType
toDest "ROOT-SW-COMPOSITION-PROTOTYPE" = RSERootSwCompositionPrototype
toDest "SYSTEM-SIGNAL" = RSESystemSignal
-- ...
toDest x = error ("Reference destination '" ++ x ++ "' not implemented.")


toBoolean "true" = True
toBoolean "false" = False

-- objects ---

toPackage e =
  Package {
    package_shortName = _shortName e,
    referenceBases = [],
    elements = List.concatMap (maybeToList . toPElement) $ optChildElems "ELEMENTS" e,
    packages = List.map toPackage $ optChildElems "AR-PACKAGES" e,
    package_variationPoint = optVariationPoint e
    }

toVariationPoint e =
  VariationPoint label btime bexpr
  where btime = toBindingTime $ (attr "BINDING-TIME") $ elem "SW-SYSCOND" e
        label = text $ elem "SHORT-LABEL" e
        bexpr = FormulaExpression $ (List.concatMap fElement (XML.elementNodes (elem "SW-SYSCOND" e)))
        fElement (XML.NodeContent s) = [RFEText (Text.unpack s)]
        fElement (XML.NodeElement c@(XML.Element { }))
          | (XML.nameLocalName (XML.elementName c)) == "SYSC-REF" = [RFEReference (aref c)]
          | otherwise = error ("Unexpected VARIATION-POINT condition: " ++ (show c))
        fElement x = error ("Unexpected VARIATION-POINT condition: " ++ (show x))

toBindingTime "CODE-GENERATION-TIME" = CodeGenerationTime
toBindingTime "BLUEPRINT-DERIVATION-TIME" = BlueprintDerivationTime
toBindingTime "SYSTEM-DESIGN-TIME" = SystemDesignTime
toBindingTime "PRE-COMPILE-TIME" = PreCompileTime
toBindingTime "POST-BUILD" = PostBuild
toBindingTime "LINK-TIME" = LinkTime


toPElement e
  | isElem "APPLICATION-SW-COMPONENT-TYPE" e = Just (ApplicationSwComponentTypePElement (toApplicationSwComponentType e))
  | isElem "PARAMETER-SW-COMPONENT-TYPE" e = Just (ParameterSwComponentTypePElement (toParameterSwComponentType e))
  | isElem "SWC-IMPLEMENTATION" e = Just (SwcImplementationPElement (toSwcImplementation e))
  | isElem "SENDER-RECEIVER-INTERFACE" e = Just (SenderReceiverInterfacePElement (toSenderReceiverInterface e))
  | isElem "SENSOR-ACTUATOR-SW-COMPONENT-TYPE" e = Just (SensorActuatorSwComponentTypePElement (toSensorActuatorSwComponentType e))
  | isElem "NV-BLOCK-SW-COMPONENT-TYPE" e = Just (NvBlockSwComponentTypePElement (toNvBlockSwComponentType e))
  | isElem "COMPOSITION-SW-COMPONENT-TYPE" e = Just (CompositionSwComponentTypePElement (toCompositionSwComponentType e))
  | isElem "PORT-INTERFACE-MAPPING-SET" e = Just (PortInterfaceMappingSetPElement (toPortInterfaceMappingSet e))
  | isElem "FLAT-MAP" e = Just (FlatMapPElement (toFlatMap e))
  | isElem "SYSTEM" e = Just (SystemPElement (toSystem e))
  | isElem "MODE-SWITCH-INTERFACE" e = Just (ModeSwitchInterfacePElement (toModeSwitchInterface e))
  | isElem "CLIENT-SERVER-INTERFACE" e = Just (ClientServerInterfacePElement (toClientServerInterface e))
  | isElem "PARAMETER-INTERFACE" e = Just (ParameterInterfacePElement (toParameterInterface e))
  | isElem "NV-DATA-INTERFACE" e = Just (NvDataInterfacePElement (toNvDataInterface e))
  | isElem "TRIGGER-INTERFACE" e = Just (TriggerInterfacePElement (toTriggerInterface e))
  | isElem "SW-SYSTEMCONST" e = Just (SwSystemconstPElement (toSwSystemconst e))
  | isElem "SW-SYSTEMCONSTANT-VALUE-SET" e = Nothing -- TODO?
  | isElem "SYSTEM-SIGNAL" e = Nothing -- TODO?
  -- ignore, not interesting I think:
  | isElem "ECUC-MODULE-CONFIGURATION-VALUES" e = Nothing -- ignore, not interesting
  | isElem "IMPLEMENTATION-DATA-TYPE" e = Nothing
  | isElem "SW-BASE-TYPE" e = Nothing
  | isElem "DATA-CONSTR" e = Nothing
  | isElem "APPLICATION-PRIMITIVE-DATA-TYPE" e = Nothing
  | isElem "APPLICATION-ARRAY-DATA-TYPE" e = Nothing
  | isElem "MODE-DECLARATION-GROUP" e = Nothing
  | isElem "DATA-TYPE-MAPPING-SET" e = Nothing
  | isElem "COMPU-METHOD" e = Nothing
  | isElem "PREDEFINED-VARIANT" e = Nothing
  | isElem "SWC-TIMING" e = Nothing
  | isElem "BSW-MODULE-ENTRY" e = Nothing
  | isElem "BSW-MODULE-DESCRIPTION" e = Nothing
  | isElem "ECUC-MODULE-DEF" e = Nothing
  | isElem "BSW-IMPLEMENTATION" e = Nothing
  | isElem "CAN-CLUSTER" e = Nothing
  | isElem "I-SIGNAL-I-PDU" e = Nothing
  | isElem "I-SIGNAL" e = Nothing
  | isElem "CAN-FRAME" e = Nothing
  | isElem "ECU-INSTANCE" e = Nothing
  | isElem "ECUC-DEFINITION-COLLECTION" e = Nothing
  | isElem "ECUC-VALUE-COLLECTION" e = Nothing
                  
  | otherwise = error ("Nothing implemented for packageable element '" ++ (name_unpack (XML.elementName e)) ++ "'.")

toParameterSwComponentType e = ParameterSwComponentType {
  parameterSwComponentType_swComponentType = toSwComponentType e,
  constantMapping = [], -- TODO
  parameterSwComponentType_dataTypeMapping = [], -- TODO
  parameterSwComponentType_instantiationDataDefProps = [] -- TODO
  }

toSwSystemconst e = SwSystemconst {
  shortName_0 = _shortName e,
  swSystemconst_variationPoint = optVariationPoint e,
  constSwDataDefProps = Nothing -- TODO
  }

toNvDataInterface e = NvDataInterface {
  nvDataInterface_portInterface = toPortInterface_ e,
  nvData = map toVariableDataPrototype $ optChildElems "NV-DATAS" e
  }

toTriggerInterface e = TriggerInterface {
  triggerInterface_portInterface = toPortInterface_ e,
  triggerInterface_trigger = map toTrigger $ optChildElems "TRIGGERS" e
  }

toTrigger e = Trigger {
  trigger_shortName = _shortName e,
  swImplPolicy = Nothing, -- TODO,
  triggerPeriod = Nothing -- TODO
  }

toParameterInterface e = ParameterInterface {
  parameterInterface_portInterface = toPortInterface_ e,
  parameter = map toParameterDataPrototype $ optChildElems "PARAMETERS" e
  }

toParameterDataPrototype e = ParameterDataPrototype {
  parameterDataPrototype_shortName = _shortName e,
  parameterDataPrototype_initValue = Nothing, -- TODO
  parameterDataPrototype_type = typeRef e,
  parameterDataPrototype_variationPoint = optVariationPoint e
  }

toClientServerInterface e = ClientServerInterface {
  clientServerInterface_portInterface = toPortInterface_ e,
  clientServerInterface_operation = map toClientServerOpertation $ optChildElems "OPERATIONS" e,
  clientServerInterface_possibleError = [] -- TODO
  }

toClientServerOpertation e = ClientServerOperation {
  clientServerOperation_shortName = _shortName e,
  clientServerOperation_argument = [], -- TODO
  clientServerOperation_possibleError = [], -- TODO
  clientServerOperation_variationPoint = optVariationPoint e
  }

toModeSwitchInterface e = ModeSwitchInterface {
  modeSwitchInterface_portInterface = toPortInterface_ e,
  modeGroup = toModeDeclarationGroupPrototype $ elem "MODE-GROUP" e
  }

toModeDeclarationGroupPrototype e = ModeDeclarationGroupPrototype {
  modeDeclarationGroupPrototype_shortName = _shortName e,
  swCalibrationAccess = fmap (toSwCalibrationAccessEnum . text) $ optElem "SW-CALIBRATION-ACCESS" e,
  modeDeclarationGroupPrototype_type = typeRef e
  }

toSwCalibrationAccessEnum "READ-ONLY" = SCAReadOnly
toSwCalibrationAccessEnum "NOT-ACCESSIBLE" = SCANotAccessible
toSwCalibrationAccessEnum "READ-WRITE" = SCAReadWrite

toSystem e = System {
  systemDocumentation = [], -- TODO
  ecuExtractVersion =  fmap text $ optElem "ECU-EXTRACT-VERSION" e,
  fibexElement = [], -- TODO map aref $ optChildElems "FIBEX-ELEMENTS" e,
  system_mapping = map toSystemMapping $ optChildElems "MAPPINGS" e,
  pncVectorLength = Nothing, -- TODO
  pncVectorOffset = Nothing, -- TODO
  rootSoftwareComposition = map toRootSwCompositionPrototype $ optChildElems "ROOT-SOFTWARE-COMPOSITIONS" e,
  systemVersion = Nothing, -- TODO
  system_shortName = _shortName e,
  system_variationPoint = optVariationPoint e
  }

toSystemMapping e = SystemMapping {
  systemMapping_shortName = _shortName e,
  dataMapping = map toDataMapping $ optChildElems "DATA-MAPPINGS" e,
  systemMapping_variationPoint = optVariationPoint e,
  swMapping = [], -- TODO present, but not interesintg? map toSwcToEcuMapping $ optChildElems "SW-MAPPINGS",
  -- TODO, but not used in example:
  ecuResourceMapping = [],
  systemMapping_mappingConstraint = [],
  pncMapping = [],
  resourceEstimation = [],
  signalPathConstraint = [],
  swImplMapping = []
  }

toDataMapping e
  | isElem "SENDER-RECEIVER-TO-SIGNAL-MAPPING" e =
    SenderReceiverToSignalMapping {
      dataMapping_ = toDataMapping_ e,
      mapping_dataElement = toDataElementIRef $ elem "DATA-ELEMENT-IREF" e,
      systemSignal = aref $ elem "SYSTEM-SIGNAL-REF" e
      }
    -- TODO some more kinds
  | otherwise = error ("Nothing implemented for data mapping '" ++ (name_unpack (XML.elementName e)) ++ "'.")

toDataElementIRef e =
  -- note the order is important (more relevant than the order or
  -- elements in the xml)
  iref ["CONTEXT-COMPOSITION-REF", "CONTEXT-COMPONENT-REF", "CONTEXT-PORT-REF"] "TARGET-DATA-PROTOTYPE-REF" e

toDataMapping_ e = DataMapping {
  communicationDirectionType = Nothing, -- TODO
  dataMapping_variationPoint = optVariationPoint e
  }

toRootSwCompositionPrototype e = RootSwCompositionPrototype {
  rootSwCompositionPrototype_shortName = _shortName e,
  calibrationParameterValueSet = map aref $ optChildElems "CALIBRATION-PARAMETER-VALUE-SET-REFS" e,
  flatMap = fmap aref $ optElem "FLAT-MAP-REF" e,
  softwareComposition = aref $ elem "SOFTWARE-COMPOSITION-TREF" e,
  rootSwCompositionPrototype_variationPoint = optVariationPoint e
  }

toFlatMap e = FlatMap {
  flatMap_shortName = _shortName e,
  flatMap_flatInstanceDescriptor = map toFlatInstanceDescriptor $ optChildElems "INSTANCES" e,
  flatMap_variationPoint = optVariationPoint e
  }

toFlatInstanceDescriptor e = FlatInstanceDescriptor {
  flatInstanceDescriptor_shortName = _shortName e
  -- TODO more?
}

toPortInterfaceMappingSet e = PortInterfaceMappingSet {
  portInterfaceMappingSet_shortName = _shortName e,
  portInterfaceMappings = map toPortInterfaceMapping $ optChildElems "PORT-INTERFACE-MAPPINGS" e,
  portInterfaceMappingSet_variationPont = optVariationPoint e
}

toPortInterfaceMapping_ e = PortInterfaceMapping {
  portInterfaceMapping_shortName = _shortName e,
  portInterfaceMapping_variationPoint = optVariationPoint e
  }

toPortInterfaceMapping e
  | isElem "VARIABLE-AND-PARAMETER-INTERFACE-MAPPING" e =
    VariableAndParameterInterfaceMapping {
      portInterfaceMapping = toPortInterfaceMapping_ e,
      dataPrototypeMapping = map toDataPrototypeMapping $ optChildElems "DATA-MAPPINGS" e
      }
  | otherwise = error ("Nothing implemented for port interface mapping '" ++ (name_unpack (XML.elementName e)) ++ "'.")

toDataPrototypeMapping e = DataPrototypeMapping {
  firstDataPrototype = aref $ elem "FIRST-DATA-PROTOTYPE-REF" e,
  secondDataPrototype = aref $ elem "SECOND-DATA-PROTOTYPE-REF" e,
  subElementMapping = map toSubElementMapping $ optChildElems "SUB-ELEMENT-MAPPINGS" e,
  textTableMapping = map toTextTableMapping $ optChildElems "TEXT-TABLE-MAPPINGS" e
  }

toSubElementMapping e = undefined {- TODO, but SubElementRef is more work than this:
SubElementMapping {
  firstElement = map aref $ optChildElems "FIRST-ELEMENTS" e,
  secondElement = map aref $ optChildElems "SECOND-ELEMENTS" e,
  subTextTableMapping = map toTextTableMapping $ optChildElems "TEXT-TABLE-MAPPINGS" e
}
-}

toTextTableMapping e = TextTableMapping {
  identicalMapping = maybe False (toBoolean . text) $ optElem "IDENTICAL-MAPPING" e, -- default guessed
  mappingDirection = fmap (toMappingDirectionEnum $ text) $ optElem "MAPPING-DIRECTION" e,
  valuePair = map toTextTableValuePair $ optChildElems "VALUE-PAIRS" e
  }

toMappingDirectionEnum = undefined -- TODO

toTextTableValuePair = undefined -- TODO

toCompositionSwComponentType e = CompositionSwComponentType {
  compositionSwComponentType_swComponentType = toSwComponentType e,
  compositionSwComponentType_component = map toSwComponentPrototype $ optChildElems "COMPONENTS" e,
  connectors = map toSwConnector $ optChildElems "CONNECTORS" e,
  compositionSwComponentType_constantValueMapping = [], -- TODO
  compositionSwComponentType_dataTypeMapping = [], -- TODO
  instantiationRTEEventProps = [] -- TODO
  }

toSwComponentPrototype e = SwComponentPrototype {
  swComponentPrototype_shortName = _shortName e,
  swComponentPrototype_type = typeRef e,
  swComponentPrototype_variationPoint = optVariationPoint e
  }

toSwConnector e
  | isElem "DELEGATION-SW-CONNECTOR" e = DelegationSwConnector {
    swConnector = toSwConnector_ e,
    innerPort = toPOrRPortPrototypeIRef $ firstElem $ elem "INNER-PORT-IREF" e,
    outerPort = aref $ elem "OUTER-PORT-REF" e
    }
  | isElem "ASSEMBLY-SW-CONNECTOR" e = AssemblySwConnector {
    swConnector = toSwConnector_ e,
    provider = fmap toPPortPrototypeIRef $ optElem "PROVIDER-IREF" e,
    requester = fmap toRPortPrototypeIRef $ optElem "REQUESTER-IREF" e
    }
  | isElem "PASS-THROUGH-SW-CONNECTOR" e = PassThroughSwConnector {
    swConnector = toSwConnector_ e,
    providedOuterPort = aref $ elem "PROVIDED-OUTER-PORT-REF" e,
    requiredOuterPort = aref $ elem "REQUIRED-OUTER-PORT-REF" e
    }

toSwConnector_ e = SwConnector {
  swConnector_shortName = _shortName e,
  swConnector_mapping = fmap aref $ optElem "MAPPING-REF" e,
  swConnector_variationPoint = optVariationPoint e
  }

toPOrRPortPrototypeIRef e
  | isElem "P-PORT-IN-COMPOSITION-INSTANCE-REF" e =
    toPPortPrototypeIRef e
  | isElem "R-PORT-IN-COMPOSITION-INSTANCE-REF" e =
    toRPortPrototypeIRef e
  | otherwise = error ("Unknown PortPrototypeIRef: '" ++ (name_unpack (XML.elementName e)) ++ "'.")

toPPortPrototypeIRef e = -- P or PR
  iref ["CONTEXT-COMPONENT-REF"] "TARGET-P-PORT-REF" e

toRPortPrototypeIRef e = -- R or PR
  iref ["CONTEXT-COMPONENT-REF"] "TARGET-R-PORT-REF" e

toNvBlockSwComponentType e = NvBlockSwComponentType {
  nvBlockSwComponentType_atomicSwComponentType = toAtomicSwComponentType e,
  nvBlockDescriptor = [] -- TODO
  }

toSensorActuatorSwComponentType e = SensorActuatorSwComponentType {
  sensorActuatorSwComponentType_atomicSwComponentType = toAtomicSwComponentType e,
  sensorActuator = fmap aref $ optElem "SENSOR-ACTUATOR-REF" e
  }

toSenderReceiverInterface e = SenderReceiverInterface {
  senderReceiverInterface_portInterface = toPortInterface_ e,
  -- dataElements must be "VARIABLE-DATA-PROTOTYPE"
  dataElement = List.map toVariableDataPrototype $ optChildElems "DATA-ELEMENTS" e,
  invalidationPolicy = [] -- TODO
  }

toPortInterface_ e = PortInterface {
  portInterface_shortName = _shortName e,
  portInterface_variationPoint = optVariationPoint e,
  isService = maybe False toBoolean (optAttr "IS-SERVICE" e),
  serviceKind = Nothing -- TODO
  }

toVariableDataPrototype e = VariableDataPrototype {
  variableDataPrototype_shortName = _shortName e,
  variableDataPrototype_type = typeRef e,
  variableDataPrototype_initValue = Nothing, -- TODO
  variableDataPrototype_variationPoint = optVariationPoint e
  }

toSwcImplementation e = SwcImplementation {
  implementation = toImplementation e,
  behavior = aref $ elem "BEHAVIOR-REF" e,
  perInstanceMemorySize = [], -- TODO
  requiredRTEVendor = Nothing -- TODO
  }

-- FIXME: The XSD has everything optional, and annotated as
-- "pureMM.minOccurs=1", for unknown reasons. And ARXML examples usually
-- define nothing for those properties!?
toImplementation e = Implementation {
  implementation_shortName = _shortName e,
  implementation_variationPoint = optVariationPoint e,
  -- TODO: ff
  buildActionManifest = [],
  codeDescriptor = [],
  compiler = [],
  generatedArtifact = [],
  hwElement = [],
  linker = [],
  programmingLanguage = (),
  requiredArtifact = [],
  requiredGeneratorTool = [],
  resourceConsumption = (),
  swVersion = "", -- pureMM.minOccurs=1 ???
  swcBswMapping = Nothing,
  usedCodeGenerator = Nothing,
  vendorId = 0 -- pureMM.minOccurs=1 ???
  }

toApplicationSwComponentType e = ApplicationSwComponentType {
  applicationSwComponentType_atomicSwComponentType = toAtomicSwComponentType e
  }

toAtomicSwComponentType e = AtomicSwComponentType {
  atomicSwComponentType_swComponentType = toSwComponentType e,
  internalBehavior = [], -- TODO
  symbolProps = Nothing -- TODO
  }

toSwComponentType e = SwComponentType {
  swComponentType_shortName = _shortName e,
  swComponentType_variationPoint = optVariationPoint e,
  consistencyNeeds = [], -- TODO
  port = map toPortPrototype $ optChildElems "PORTS" e,
  portGroup = [], -- TODO
  swComponentDocumentation = [], -- TODO
  unitGroup = [] -- TODO
  }

toPortPrototype_ e =
  PortPrototype {
    portPrototype_shortName = _shortName e,
    portPrototype_variationPoint = (optVariationPoint e),
    -- TODO ff
    clientServerAnnotation = [],
    delegatedPortAnnotation = Nothing,
    ioHwAbstractionServerAnnotation = [],
    modePortAnnotation = [],
    nvDataPortAnnotation = [],
    parameterPortAnnotation = [],
    senderReceiverAnnotation = [],
    triggerPortAnnotation = []
    }

toPortPrototype e
  | isElem "R-PORT-PROTOTYPE" e =
    RPortPrototype {
      portPrototype = toPortPrototype_ e,
      requiredComSpec = [], -- TODO
      requiredInterface = aref $ elem "REQUIRED-INTERFACE-TREF" e
      }
  | isElem "P-PORT-PROTOTYPE" e =
    PPortPrototype {
      portPrototype = toPortPrototype_ e,
      providedComSpec = [], -- TODO
      providedInterface = aref $ elem "PROVIDED-INTERFACE-TREF" e
      }
  | isElem "PR-PORT-PROTOTYPE" e =
    PRPortPrototype {
      portPrototype = toPortPrototype_ e,
      providedRequiredInterface = aref $ elem "PROVIDED-REQUIRED-INTERFACE-TREF" e,
      requiredComSpec = [], -- TODO
      providedComSpec = [] -- TODO
      }
