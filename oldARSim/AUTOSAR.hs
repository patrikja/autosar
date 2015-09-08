{-
Copyright (c) 2014-2015, Johan Nordlander, Jonas Duregård, Michał Pałka,
                         Patrik Jansson, Josef Svenningsson and
                         Active Group GmbH
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

{-# LANGUAGE MultiParamTypeClasses, GADTs, StandaloneDeriving, KindSignatures, FlexibleInstances, FlexibleContexts #-}
module AUTOSAR where

import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Exception (assert)
import Control.Monad
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import System.IO (stdout)

putPretty p =
  do v <- p
     -- _ <- PP.putDoc (PP.renderPretty 0.8 120 v)
     _ <- PP.displayIO stdout (PP.renderPretty 0.8 120 (PP.pretty v))
     return (show v)

instance Show ReferenceTree where
  showsPrec d tree       = PP.displayS (PP.renderPretty 0.8 120 (PP.pretty tree))

class Referable a where
        shortName                   :: a -> String

type ARPath = [String]
appendPath :: ARPath -> String -> ARPath
appendPath path n = path ++ [n]

pathShortName :: ARPath -> String
pathShortName p = last p

data Ref a =
  -- ReferrableSubTypesEnum is not quite the right type for the "DEST"
  -- values, but probably sufficient for out requirements:
  AR ReferrableSubtypesEnum ARPath -- Absolute references
  deriving (Eq, Ord, Show)

-- Note: it's questionable if dest types should be compared in '=='

refPath (AR _ path) = path
refType (AR d _) = d
-- ShortNames should be compared case-insensitively, according to
-- docs; but probably not relevant in our examples.
-- (GenericStructureTemplate 3.2)

refCast :: Ref a -> Ref b
refCast (AR d l) = AR d l

data AnyRef where
  AnyRef :: Ref a -> AnyRef

instance Eq AnyRef where
  (==) (AnyRef a) (AnyRef b) = (refCast a) == (refCast b)

instance Ord AnyRef where
  (<=) (AnyRef a) (AnyRef b) = (refCast a) <= (refCast b)

anyRefPath (AnyRef r) = refPath r

deriving instance Show AnyRef

data CRef where
  CAR :: Ref a -> CRef
  
deriving instance Show CRef

instance Eq CRef where
  (==) (CAR (AR d1 l1)) (CAR (AR d2 l2)) = (d1 == d2) && (l1 == l2)

instance Ord CRef where
  (<=) (CAR (AR d1 l1)) (CAR (AR d2 l2)) = (d1 <= d2) && (l1 <= l2)

data IRef a = -- context, target
  IAR [CRef] (Ref a)
  deriving (Eq, Show, Ord)

-- deriving instance Ord (IRef a)

iRefCast :: IRef a -> IRef b
iRefCast (IAR crs r) = IAR crs (refCast r)

iRefTarget :: IRef a -> Ref a
iRefTarget (IAR _ r) = r

iRefContext :: IRef a -> [CRef]
iRefContext (IAR c r) = c

data AnyIRef where
  AnyIRef :: IRef a -> AnyIRef

deriving instance Show AnyIRef
instance Eq AnyIRef where
  (==) (AnyIRef i1) (AnyIRef i2) = (iRefCast i1) == (iRefCast i2)

-- name resolution

-- resolve a path *within* a package (i.e. the short name of p is not part of the path)
derefElement :: Ref t -> Package -> [Element]
derefElement (AR dest path) p =
  lookupInPackage path p

derefAnyElement :: AnyRef -> Package -> [Element]
derefAnyElement r p = lookupInPackage (anyRefPath r) p

lookupInPackage path p =
  -- one of them should be empty:
  (concatMap (lookupPackage path) (packages p)) ++ (concatMap (lookupElement path) (map toElement (elements p)))

-- follow down the road, iff shortName of Package = first part path
lookupPackage (pe : r) p
  | pe == (package_shortName p) =
    if null r then
      error "References to packages not supported."
    else
      lookupInPackage r p
  | otherwise = []

lookupElement (pe : r) e
  | pe == (shortName e) =
    if null r then
      [e] -- found it
    else
      concatMap (lookupElement r) (aggregates e)
  | otherwise = []

-----------------------------------------------------------------
-- Cross-cutting stereotypes
-----------------------------------------------------------------

-- Section 4.7 in Generic Structure Template, Document Version 3.5.0

data Value =
    StringValue String
  | FloatValue Double
  -- either -2147483648 .. +4294967295 or -9223372036854775808 .. +18446744073709551615
  | IntValue Integer
  deriving (Show, Eq)

valueIsTrue :: Value -> Bool
valueIsTrue (StringValue _) = False
valueIsTrue (FloatValue x) = (x /= 0.0) && (x /= -0.0)
valueIsTrue (IntValue x) = x /= 0

data FormulaExpression =
    Literal Value
  | Reference (Ref SwSystemconst)
  | TextualExpression String
  -- ... very incomplete at this point
  deriving (Show, Eq)

data RawFormulaElement =
    RFEReference (Ref SwSystemconst)
  | RFEText String
  deriving (Show, Eq, Ord)

type RawFormulaExpression = [RawFormulaElement]

type Environment = Map [String] Value -- always map from absolute

data BindingExpression =
  FormulaExpression RawFormulaExpression
  deriving (Show, Eq, Ord)

class Evaluable e where
  evaluate :: Environment -> e -> Value

instance Evaluable BindingExpression where
  -- FIXME or REMOVE: would need: parseRFE :: RawFormulaExpression -> FormulaExpression
  evaluate e (FormulaExpression fe) = undefined -- evaluate e fe

instance Evaluable FormulaExpression where
  evaluate _ (Literal v) = v
  evaluate env (Reference (AR _ r)) = fromJust (Map.lookup r env)

data Variant a = 
    UnresolvedVariant VariationPoint a
  | ConfirmedVariant a
  deriving (Show, Eq)
  
variantValue (UnresolvedVariant _ x) = x
variantValue (ConfirmedVariant x) = x

data BindingTime =
    BlueprintDerivationTime
  | SystemDesignTime
  | CodeGenerationTime
  | PreCompileTime
  | PostBuild
  | LinkTime
  deriving (Eq, Ord, Enum, Show)

data VariationPoint =
    -- the String shortLabel might be empty/or should be optional
    VariationPoint String BindingTime BindingExpression
  | VCombinedVariationPoint VariationPoint VariationPoint
  deriving (Show, Eq, Ord)

variationPoint_shortLabel (VariationPoint label _ _) = label -- probably not needed for a combined one?

combineVariationPoints = VCombinedVariationPoint

data Variable a = Variable [Variant a]
  deriving (Show, Eq)

type Var a = [a]

class HasVariationPoint t where 
   variationPoint :: t -> Maybe VariationPoint

data VRef t where
  VRef :: {
    ref :: Ref t,
    vref_variationPoint :: Maybe VariationPoint
  } -> VRef t

deriving instance Show (VRef t)
deriving instance Eq (VRef t)

instance HasVariationPoint (VRef t) where
  variationPoint = vref_variationPoint

data Dereferenced a =
    DeRef AnyRef a
  | DeIRef AnyIRef a
  | Direct a

deriving instance Show a => Show (Dereferenced a)

referenced :: Dereferenced a -> a
referenced (DeRef _ x) = x
referenced (DeIRef _ x) = x
referenced (Direct x) = x

data ReferenceTree =
    -- the idea is that the elements will be sans packages ... well, they always are
    -- the map key is the short name of all elements in all the ReferenceTreeLevel
    -- each ReferenceTreeLevel represents a merge in the hierarchy
    ReferenceTree (Map String [ReferenceTreeLevel])
    -- deriving Show

instance PP.Pretty ReferenceTree where
  pretty rt =
    ppTree [] rt
    where ppTree prefix (ReferenceTree mp) = PP.vsep (map (ppLevels prefix) (Map.toList mp))

          ppLevels prefix (n, [lvl]) = (ppPath names) PP.<+> (ppLevel names lvl) -- one element level
            where names = (prefix ++ [n])
          ppLevels prefix (n, lvls) = (PP.blue (ppPath names)) PP.<> PP.linebreak PP.<> (PP.indent 2 (PP.sep (map (ppLevel names) lvls)))
            where names = (prefix ++ [n])

          ppLevel prefix (ReferenceTreeLevel v tree) =
            (ppVar v) PP.<$> (ppTree prefix tree) -- (PP.indent 2 )

          ppVar (Variable [derefVariant]) = PP.empty -- ppDerefVariant derefVariant
          ppVar (Variable derefVariants) = PP.softline PP.<> (PP.onblack (PP.list (map ppDerefVariant derefVariants))) -- special display for mergers
          ppDerefVariant (UnresolvedVariant vp x) = {-PP.blue (PP.text "VP xx") PP.<+>-} (ppDeref x)
          ppDerefVariant (ConfirmedVariant x) = ppDeref x
          ppDeref (DeRef r _) = (PP.text "ref:") PP.<+> (PP.red (PP.pretty r))
          ppDeref (DeIRef (AnyIRef (IAR ctx r)) _) = (PP.text "iref:") PP.<+> (PP.green (PP.pretty r)) -- TODO more on context?!
          ppDeref (Direct x) = PP.text (shortName x)

zeroReferenceTree = ReferenceTree Map.empty

unReferenceTree (ReferenceTree m) = m

data ReferenceTreeLevel =
  ReferenceTreeLevel (Variable (Dereferenced Element)) ReferenceTree
  deriving Show

instance PP.Pretty AnyRef where
  pretty (AnyRef r) = PP.pretty r

instance PP.Pretty (Ref a) where
  pretty (AR target path) = (PP.text (show target)) PP.<+> (ppPath path)

ppPath path = foldl (\d1 d2 -> d1 PP.<> (PP.text "/") PP.<> d2) (PP.text $ head path) (map PP.text (tail path))

class HasVariableSubElements t where
  variableSubElements :: t -> [Element]

class HasSubElements t where
  aggregates :: t -> [Element]
  references :: t -> [AnyRef]
  instantiates :: t -> [AnyIRef]

class ToElement t where
  toElement :: t -> Element

instance ToElement Element where
  toElement = id

resolveBTopLevelM :: IO [Package] -> IO ReferenceTree
resolveBTopLevelM = liftM resolveBTopLevel

resolveBTopLevel :: [Package] -> ReferenceTree
resolveBTopLevel ps =
  package_resolveB topLevelPackage topLevelVariationPoint topLevelPackage
  where topLevelPackage = Package {
          package_shortName = "",
          referenceBases = [],
          elements = [],
          packages = ps,
          package_variationPoint = Nothing
          }
        topLevelVariationPoint = -- ???
          VariationPoint "" BlueprintDerivationTime (FormulaExpression [RFEText ""])


resolveB :: (HasSubElements t, HasVariationPoint t, Referable t, ToElement t) =>
              Package -> VariationPoint -> t -> ReferenceTree
resolveB p vp v = resolveB' Direct p vp v

resolveB' :: (HasSubElements t, HasVariationPoint t, Referable t, ToElement t) =>
               (Element -> Dereferenced Element) -> Package -> VariationPoint -> t -> ReferenceTree
resolveB' wrap p vp v =
  let vr = case variationPoint v of
             Nothing -> ConfirmedVariant (wrap (toElement v))
             Just vp' -> UnresolvedVariant (vp `combineVariationPoints` vp') (wrap (toElement v))
  in ReferenceTree
      (Map.singleton (shortName v)
       [ReferenceTreeLevel
        (Variable [vr])
        -- FIXME: shouldn't this be the new VP?
        (mergesReferenceTrees ((map (resolveB p vp) (aggregates v)) ++
                               (map (resolveBAnyRef p vp) (references v)) ++
                               (map (resolveBAnyIRef p vp) (instantiates v))
                              ))])

resolveBAnyRef :: Package -> VariationPoint -> AnyRef -> ReferenceTree
resolveBAnyRef p vp ar@(AnyRef r) = resolveBRef' (DeRef ar) p vp r

resolveBAnyIRef :: Package -> VariationPoint -> AnyIRef -> ReferenceTree
resolveBAnyIRef p vp air@(AnyIRef (IAR _ r)) = resolveBRef' (DeIRef air) p vp r

resolveBRef' :: (Element -> Dereferenced Element) -> Package -> VariationPoint -> Ref t -> ReferenceTree
resolveBRef' wrap p vp r =
  let es = derefElement r p
      ts = map (resolveB' wrap p vp) es
  in
   {-if null es then
     error ("Referenced element not found: " ++ (show r))
   else-} -- FIXME; temporarily removed
     mergesReferenceTrees ts

mapV :: (a -> b) -> [Variant a] -> [Variant b]
mapV f [] = []
mapV f ((UnresolvedVariant vp a) : r) = (UnresolvedVariant vp (f a)) : (mapV f r)
mapV f ((ConfirmedVariant a) : r) = (ConfirmedVariant (f a)) : (mapV f r)


-- we require this to be reflexive, symmetric, and transitive
elementsMergable :: Element -> Element -> Bool

-- "CPT can be merged if same SWC is addressed"?
elementsMergable (SwComponentPrototypeElement cpt1) (SwComponentPrototypeElement cpt2) =
  swComponentPrototype_type cpt1 == swComponentPrototype_type cpt2

-- "Ports can be merged if same interface is addressed"
elementsMergable (PortPrototypeElement (rp1@RPortPrototype {})) (PortPrototypeElement (rp2@RPortPrototype {})) =
  requiredInterface rp1 == requiredInterface rp2
elementsMergable (PortPrototypeElement (pp1@PPortPrototype {})) (PortPrototypeElement (pp2@PPortPrototype {})) =
  providedInterface pp1 == providedInterface pp2 -- TODO correct?
elementsMergable (PortPrototypeElement (prp1@PRPortPrototype {})) (PortPrototypeElement (prp2@PRPortPrototype {})) =
  (providedRequiredInterface prp1 == providedRequiredInterface prp2) -- TODO correct?
elementsMergable (PortPrototypeElement _) (PortPrototypeElement _) = False

-- "Connectors can be merged if same end points are addressed."
elementsMergable (SwConnectorElement (asc1@AssemblySwConnector {})) (SwConnectorElement (asc2@AssemblySwConnector {})) =
  (provider asc1 == provider asc2) && (requester asc1 == requester asc2)
elementsMergable (SwConnectorElement (dsc1@DelegationSwConnector {})) (SwConnectorElement (dsc2@DelegationSwConnector {})) =
  (innerPort dsc1 == innerPort dsc2) && (innerPort dsc1 == innerPort dsc2)
elementsMergable (SwConnectorElement (ptsc1@PassThroughSwConnector {})) (SwConnectorElement (ptsc2@PassThroughSwConnector {})) =
  (providedOuterPort ptsc1 == providedOuterPort ptsc2) && (requiredOuterPort ptsc1 == requiredOuterPort ptsc2)
elementsMergable (SwConnectorElement _) (SwConnectorElement _) = False

elementsMergable e1 e2 = False

-- either transform a single list element or the whole list
maybeMergeInto :: (a -> Maybe a) -> ([a] -> [a])-> [a] -> [a]
maybeMergeInto t1 fallback lis0 =
  let loop lis rev =
        case lis of
          [] -> fallback lis0
          x:xs -> case t1 x of
                    Nothing -> loop xs (x:rev)
                    Just x' -> (reverse rev) ++ (x':xs)
  in loop lis0 []

adjoinReferenceTreeLevel :: String -> ReferenceTree -> ReferenceTreeLevel -> ReferenceTree
adjoinReferenceTreeLevel n r@(ReferenceTree mp) rtl@(ReferenceTreeLevel (Variable vs) rt)
  | null vs =
      let t Nothing = Just [rtl]
          t (Just (rtl':rtls)) = Just ((mergeReferenceTreeLevels rtl' rtl):rtls) -- FIXME: questionable - why the first?
      in ReferenceTree (Map.alter t n mp)
  | otherwise =
      let t Nothing = Just [rtl]
          t (Just rtls) =
              let el0 = referenced (variantValue (head vs))
                  rtls' = maybeMergeInto (\ (rtl'@(ReferenceTreeLevel (Variable vs') rt')) ->
                                           if elementsMergable el0 (referenced (variantValue (head vs')))
                                           then Just (mergeReferenceTreeLevels rtl rtl')
                                           else Nothing)
                                         (rtl:)
                                         rtls
              in Just rtls'
      in ReferenceTree (Map.alter t n mp)

mergeReferenceTreeLevels :: ReferenceTreeLevel -> ReferenceTreeLevel -> ReferenceTreeLevel
mergeReferenceTreeLevels (ReferenceTreeLevel (Variable es1) rt1) (ReferenceTreeLevel (Variable es2) rt2) =
  ReferenceTreeLevel (Variable (es1 ++ es2)) (mergeReferenceTrees rt1 rt2)

mergeReferenceTrees :: ReferenceTree -> ReferenceTree -> ReferenceTree
mergeReferenceTrees rt1 (ReferenceTree mp2) =
  -- re-merge, possibly unnecessarily expensive
  Map.foldlWithKey' (\ rt n rtls -> foldl' (adjoinReferenceTreeLevel n) rt rtls) rt1 mp2

mergesReferenceTrees :: [ReferenceTree] -> ReferenceTree
mergesReferenceTrees ts = foldr mergeReferenceTrees zeroReferenceTree ts

packageableElement2ReferenceTree :: Package -> VariationPoint -> PackageableElement -> ReferenceTree
packageableElement2ReferenceTree p vp pe =
  case variationPoint pe of
    -- FIXME: don't propagate?
    Nothing -> resolveB p vp pe
    Just vp' -> resolveB p (vp `combineVariationPoints` vp') pe

-----------------------------------------------------------------
-- Package
-----------------------------------------------------------------

data Package = Package {
        package_shortName               :: String,
        referenceBases                  :: [ReferenceBase],
        elements                        :: Var PackageableElement,  -- atpVariation, atpSplitable, 0..*
        packages                        :: Var Package,  -- atpVariation, atpSplitable, 0..*
        package_variationPoint          :: Maybe VariationPoint
    }

deriving instance Show Package

instance Referable Package where
  shortName = package_shortName

instance HasVariationPoint Package where
  variationPoint = package_variationPoint

package_resolveB :: Package -> VariationPoint -> Package -> ReferenceTree
package_resolveB rp vp p =
    let pvs = packages p
        evs = elements p
    in ReferenceTree
         (Map.singleton (shortName p)
                        [ReferenceTreeLevel (Variable []) -- there is nothing at this level
                                            (mergesReferenceTrees ((map (packageableElement2ReferenceTree rp vp) evs) ++
                                                                   (map (package_resolveB rp vp) pvs)))])
                                  
data ReferenceBase = ReferenceBase {
        globalElement                   :: [ReferrableSubtypesEnum],
        isDefault                       :: Bool,
        -- combines isGlobal (true -> Just) & globalInPackage (only valid when isGlobal = true)
        global                          :: Maybe [Package],
        -- if Nothing, the base is *this* package
        package                         :: Maybe Package,
        shortLabel                      :: String
    }

deriving instance Show ReferenceBase

data PackageableElement =
    SystemPElement System
  | SwSystemconstPElement SwSystemconst
  | EcuInstancePElement EcuInstance
  | FlatMapPElement FlatMap
  | HwElementPElement HwElement
  -- SwBaseType
  -- SwRecordLayout
  -- SwSystemconstantValueSet
  | SwcImplementationPElement SwcImplementation
  -- ViewMapSet
  -- CanCluster
  -- TtcanCluster
  -- J1939Cluster
  -- FlexrayCluster
  -- LinCluster
  -- EthernetCluster
  -- CouplingElement
  -- UserDefinedCluster
  | SystemSignalPElement SystemSignal
  -- SerializationTechnology
  -- ISignal
  -- ISignalGroup
  | SystemSignalGroupPElement SystemSignalGroup
  -- ISignalPdu
  -- NmPdu
  -- NPdu
  -- XcpPdu
  -- DcmlPdu
  -- J1939DcmIPdu
  -- UserDefinedPdu
  -- UserDefinedIPdu
  -- ISignalIPduGroup
  -- PdurIPduGroup
  -- EndToEndProtectionSet
  -- MultiplexedIPdu
  -- FlexrayFrame
  -- LinFrame
  -- LinUnconditionalFrame
  -- LinSporadicFrame
  -- LinEventTriggeredFrame
  -- CanFrame
  -- SoAdRoutingGroup
  -- EthernetFrame
  -- NPdu
  -- FlexrayTpConfig
  -- CanTpConfig
  -- LinTpConfig
  -- J1939TpConfig
  -- NmConfig
  -- Gateway
  -- AliasNameSet
  -- ApplicationArrayDataType
  -- ApplicationPrimitiveDataType
  -- ApplicationRecordDataType
  -- CompuMethod
  -- FlexrayCluster
  -- FlexrayTpConfig
  -- NmPdu
  -- BlueprintMappingSet
  -- PostBuildVariantCriterion
  -- EvaluatedVariantSet
  -- PredefinedVariant
  -- SwSystemconstantValueSet
  -- BuildActionManifest
  -- AclPermission
  -- AclOperation
  -- AclRole
  -- LifeCycleStateDefinitionGroup
  -- LifeCycleInfoSet
  -- Collection
  | ParameterSwComponentTypePElement ParameterSwComponentType
  | CompositionSwComponentTypePElement CompositionSwComponentType
  | ApplicationSwComponentTypePElement ApplicationSwComponentType
  | EcuAbstractionSwComponentTypePElement EcuAbstractionSwComponentType
  | ComplexDeviceDriverSwComponentTypePElement ComplexDeviceDriverSwComponentType
  | ServiceSwComponentTypePElement ServiceSwComponentType
  | ServiceProxySwComponentTypePElement ServiceProxySwComponentType
  | NvBlockSwComponentTypePElement NvBlockSwComponentType
  | SensorActuatorSwComponentTypePElement SensorActuatorSwComponentType
  -- BswModuleDescription
  -- BswModuleEntry
  -- Documentation
  -- Unit
  | ClientServerInterfacePElement ClientServerInterface
  | TriggerInterfacePElement TriggerInterface
  | ParameterInterfacePElement ParameterInterface
  | SenderReceiverInterfacePElement SenderReceiverInterface
  | NvDataInterfacePElement NvDataInterface
  | ModeSwitchInterfacePElement ModeSwitchInterface
  -- ImplementationDataType
  -- ModeDeclarationGroup
  -- ModeDeclarationMappingSet
  | PortInterfaceMappingSetPElement PortInterfaceMappingSet
  -- EndToEndProtectionSet
  | DataTypeMappingSetPElement DataTypeMappingSet
  -- PhysicalDimension
  -- PhysicalDimensionMappingSet
  | UnitGroupPElement UnitGroup
  -- DataConstr
  | SwAddrMethodPElement SwAddrMethod
  -- SwRecordLayout
  -- InterpolationRoutineMappingSet
  -- ConstantSpecification
  | ConstantSpecificationMappingSetPElement ConstantSpecificationMappingSet
  | CalibrationParameterValueSetPElement CalibrationParameterValueSet
  -- RapidPrototypingScenario
  -- BswImplementation
  -- EcucValueCollection
  -- HwType
  -- ISignalIPdu
  | SwcBswMappingPElement SwcBswMapping

deriving instance Show PackageableElement


instance Referable PackageableElement where
  shortName = shortName . toElement

instance ToElement PackageableElement where
  toElement (SystemPElement e) = SystemElement e
  toElement (SwSystemconstPElement e) = SwSystemconstElement e
  toElement (EcuInstancePElement e) = EcuInstanceElement e
  toElement (FlatMapPElement e) = FlatMapElement e
  toElement (HwElementPElement e) = HwElementElement e
  -- SwBaseType
  -- SwRecordLayout
  -- SwSystemconstantValueSet
  toElement (SwcImplementationPElement e) = SwcImplementationElement e
  -- ViewMapSet
  -- CanCluster
  -- TtcanCluster
  -- J1939Cluster
  -- FlexrayCluster
  -- LinCluster
  -- EthernetCluster
  -- CouplingElement
  -- UserDefinedCluster
  toElement (SystemSignalPElement e) = SystemSignalElement e
  -- SerializationTechnology
  -- ISignal
  -- ISignalGroup
  toElement (SystemSignalGroupPElement e) = SystemSignalGroupElement e
  -- ISignalPdu
  -- NmPdu
  -- NPdu
  -- XcpPdu
  -- DcmlPdu
  -- J1939DcmIPdu
  -- UserDefinedPdu
  -- UserDefinedIPdu
  -- ISignalIPduGroup
  -- PdurIPduGroup
  -- EndToEndProtectionSet
  -- MultiplexedIPdu
  -- FlexrayFrame
  -- LinFrame
  -- LinUnconditionalFrame
  -- LinSporadicFrame
  -- LinEventTriggeredFrame
  -- CanFrame
  -- SoAdRoutingGroup
  -- EthernetFrame
  -- NPdu
  -- FlexrayTpConfig
  -- CanTpConfig
  -- LinTpConfig
  -- J1939TpConfig
  -- NmConfig
  -- Gateway
  -- AliasNameSet
  -- ApplicationArrayDataType
  -- ApplicationPrimitiveDataType
  -- ApplicationRecordDataType
  -- CompuMethod
  -- FlexrayCluster
  -- FlexrayTpConfig
  -- NmPdu
  -- BlueprintMappingSet
  -- PostBuildVariantCriterion
  -- EvaluatedVariantSet
  -- PredefinedVariant
  -- SwSystemconstantValueSet
  -- BuildActionManifest
  -- AclPermission
  -- AclOperation
  -- AclRole
  -- LifeCycleStateDefinitionGroup
  -- LifeCycleInfoSet
  -- Collection
  toElement (ParameterSwComponentTypePElement e) = ParameterSwComponentTypeElement e
  toElement (CompositionSwComponentTypePElement e) = CompositionSwComponentTypeElement e
  toElement (ApplicationSwComponentTypePElement e) = ApplicationSwComponentTypeElement e
  toElement (EcuAbstractionSwComponentTypePElement e) = EcuAbstractionSwComponentTypeElement e
  toElement (ComplexDeviceDriverSwComponentTypePElement e) = ComplexDeviceDriverSwComponentTypeElement e
  toElement (ServiceSwComponentTypePElement e) = ServiceSwComponentTypeElement e
  toElement (ServiceProxySwComponentTypePElement e) = ServiceProxySwComponentTypeElement e
  toElement (NvBlockSwComponentTypePElement e) = NvBlockSwComponentTypeElement e
  toElement (SensorActuatorSwComponentTypePElement e) = SensorActuatorSwComponentTypeElement e
  -- BswModuleDescription
  -- BswModuleEntry
  -- Documentation
  -- Unit
  toElement (ClientServerInterfacePElement e) = ClientServerInterfaceElement e
  toElement (TriggerInterfacePElement e) = TriggerInterfaceElement e
  toElement (ParameterInterfacePElement e) = ParameterInterfaceElement e
  toElement (SenderReceiverInterfacePElement e) = SenderReceiverInterfaceElement e
  toElement (NvDataInterfacePElement e) = NvDataInterfaceElement e
  toElement (ModeSwitchInterfacePElement e) = ModeSwitchInterfaceElement e
  -- ImplementationDataType
  -- ModeDeclarationGroup
  -- ModeDeclarationMappingSet
  toElement (PortInterfaceMappingSetPElement e) = PortInterfaceMappingSetElement e
  -- EndToEndProtectionSet
  toElement (DataTypeMappingSetPElement e) = DataTypeMappingSetElement e
  -- PhysicalDimension
  -- PhysicalDimensionMappingSet
  toElement (UnitGroupPElement e) = UnitGroupElement e
  -- DataConstr
  toElement (SwAddrMethodPElement e) = SwAddrMethodElement e
  -- SwRecordLayout
  -- InterpolationRoutineMappingSet
  -- ConstantSpecification
  toElement (ConstantSpecificationMappingSetPElement e) = ConstantSpecificationMappingSetElement e
  toElement (CalibrationParameterValueSetPElement e) = CalibrationParameterValueSetElement e
  -- RapidPrototypingScenario
  -- BswImplementation
  -- EcucValueCollection
  -- HwType
  -- ISignalIPdu
  toElement (SwcBswMappingPElement e) = SwcBswMappingElement e

instance HasSubElements PackageableElement where
  aggregates x = aggregates (toElement x)
  references x = references (toElement x)
  instantiates x= instantiates (toElement x)

-- "Anything" but packages; no corresponding AUTOSAR definition.
data Element =
    TriggerElement Trigger
  | ModeDeclarationGroupPrototypeElement ModeDeclarationGroupPrototype
  | PortPrototypeElement PortPrototype
  | SwConnectorElement SwConnector
  | RootSwCompositionPrototypeElement RootSwCompositionPrototype
  | SystemMappingElement SystemMapping
  | FibexElementElement FibexElement
  | SwcInternalBehaviorElement SwcInternalBehavior
  -- packageable elements:
  | SystemElement System
  | SwSystemconstElement SwSystemconst
  | EcuInstanceElement EcuInstance
  | FlatMapElement FlatMap
  | HwElementElement HwElement
  -- SwBaseType
  -- SwRecordLayout
  -- SwSystemconstantValueSet
  | SwcImplementationElement SwcImplementation
  -- ViewMapSet
  -- CanCluster
  -- TtcanCluster
  -- J1939Cluster
  -- FlexrayCluster
  -- LinCluster
  -- EthernetCluster
  -- CouplingElement
  -- UserDefinedCluster
  | SystemSignalElement SystemSignal
  -- SerializationTechnology
  -- ISignal
  -- ISignalGroup
  | SystemSignalGroupElement SystemSignalGroup
  -- ISignalPdu
  -- NmPdu
  -- NPdu
  -- XcpPdu
  -- DcmlPdu
  -- J1939DcmIPdu
  -- UserDefinedPdu
  -- UserDefinedIPdu
  -- ISignalIPduGroup
  -- PdurIPduGroup
  -- EndToEndProtectionSet
  -- MultiplexedIPdu
  -- FlexrayFrame
  -- LinFrame
  -- LinUnconditionalFrame
  -- LinSporadicFrame
  -- LinEventTriggeredFrame
  -- CanFrame
  -- SoAdRoutingGroup
  -- EthernetFrame
  -- NPdu
  -- FlexrayTpConfig
  -- CanTpConfig
  -- LinTpConfig
  -- J1939TpConfig
  -- NmConfig
  -- Gateway
  -- AliasNameSet
  -- ApplicationArrayDataType
  -- ApplicationPrimitiveDataType
  -- ApplicationRecordDataType
  -- CompuMethod
  -- FlexrayCluster
  -- FlexrayTpConfig
  -- NmPdu
  -- BlueprintMappingSet
  -- PostBuildVariantCriterion
  -- EvaluatedVariantSet
  -- PredefinedVariant
  -- SwSystemconstantValueSet
  -- BuildActionManifest
  -- AclPermission
  -- AclOperation
  -- AclRole
  -- LifeCycleStateDefinitionGroup
  -- LifeCycleInfoSet
  -- Collection
  | ParameterSwComponentTypeElement ParameterSwComponentType
  | CompositionSwComponentTypeElement CompositionSwComponentType
  | ApplicationSwComponentTypeElement ApplicationSwComponentType
  | EcuAbstractionSwComponentTypeElement EcuAbstractionSwComponentType
  | ComplexDeviceDriverSwComponentTypeElement ComplexDeviceDriverSwComponentType
  | ServiceSwComponentTypeElement ServiceSwComponentType
  | ServiceProxySwComponentTypeElement ServiceProxySwComponentType
  | NvBlockSwComponentTypeElement NvBlockSwComponentType
  | SensorActuatorSwComponentTypeElement SensorActuatorSwComponentType
  -- BswModuleDescription
  -- BswModuleEntry
  -- Documentation
  -- Unit
  | ClientServerInterfaceElement ClientServerInterface
  | TriggerInterfaceElement TriggerInterface
  | ParameterInterfaceElement ParameterInterface
  | SenderReceiverInterfaceElement SenderReceiverInterface
  | NvDataInterfaceElement NvDataInterface
  | ModeSwitchInterfaceElement ModeSwitchInterface
  -- ImplementationDataType
  -- ModeDeclarationGroup
  -- ModeDeclarationMappingSet
  | PortInterfaceMappingSetElement PortInterfaceMappingSet
  -- EndToEndProtectionSet
  | DataTypeMappingSetElement DataTypeMappingSet
  -- PhysicalDimension
  -- PhysicalDimensionMappingSet
  | UnitGroupElement UnitGroup
  -- DataConstr
  | SwAddrMethodElement SwAddrMethod
  -- SwRecordLayout
  -- InterpolationRoutineMappingSet
  -- ConstantSpecification
  | ConstantSpecificationMappingSetElement ConstantSpecificationMappingSet
  | CalibrationParameterValueSetElement CalibrationParameterValueSet
  -- RapidPrototypingScenario
  -- BswImplementation
  -- EcucValueCollection
  -- HwType
  -- ISignalIPdu
  | SwcBswMappingElement SwcBswMapping
  | SwComponentPrototypeElement SwComponentPrototype
  | EcuMappingElement EcuMapping
  | SwcToImplMappingElement SwcToImplMapping
  | SwcToEcuMappingElement SwcToEcuMapping
  | ConsistencyNeedsElement ConsistencyNeeds
  | PortGroupElement PortGroup
  | NvBlockDescriptorElement NvBlockDescriptor
  | ClientServerOperationElement ClientServerOperation
  | ParameterDataPrototypeElement ParameterDataPrototype
  | ExclusiveAreaElement ExclusiveArea
  | ServiceDependencyElement ServiceDependency
  | RunnableEntityElement RunnableEntity
  | PerInstanceMemoryElement PerInstanceMemory
  | RTEEventElement RTEEvent
  | VariableDataPrototypeElement VariableDataPrototype
  | ExclusiveAreaNestingOrderElement ExclusiveAreaNestingOrder
  | ArgumentDataPrototypeElement ArgumentDataPrototype
  | ServerCallPointElement ServerCallPoint
  | ParameterAccessElement ParameterAccess
  | ModeSwitchPointElement ModeSwitchPoint
  | InternalTriggeringPointElement InternalTriggeringPoint
  | VariableAccessElement VariableAccess
  | AsynchronousServerCallResultPointElement AsynchronousServerCallResultPoint
  | PortInterfaceMappingElement PortInterfaceMapping

deriving instance Show Element

instance HasVariableSubElements Element where
  variableSubElements (SystemElement s) = variableSubElements s
  -- ...

instance HasVariationPoint Element where
  variationPoint (ModeDeclarationGroupPrototypeElement x) = Nothing
  variationPoint (TriggerElement x) = Nothing
  variationPoint (PortPrototypeElement x) = variationPoint x
  variationPoint (SwConnectorElement x) = variationPoint x
  variationPoint (RootSwCompositionPrototypeElement x) = variationPoint x
  variationPoint (SystemMappingElement x) = variationPoint x
  variationPoint (FibexElementElement x) = variationPoint x
  variationPoint (SwcInternalBehaviorElement x) = variationPoint x
  -- packageable elements:
  variationPoint (SystemElement x) = variationPoint x
  variationPoint (SwSystemconstElement x) = variationPoint x
  variationPoint (EcuInstanceElement x) = variationPoint x
  variationPoint (FlatMapElement x) = variationPoint x
  variationPoint (HwElementElement x) = variationPoint x
  -- SwBaseType
  -- SwRecordLayout
  -- SwSystemconstantValueSet
  variationPoint (SwcImplementationElement x) = variationPoint x
  -- ViewMapSet
  -- CanCluster
  -- TtcanCluster
  -- J1939Cluster
  -- FlexrayCluster
  -- LinCluster
  -- EthernetCluster
  -- CouplingElement
  -- UserDefinedCluster
  variationPoint (SystemSignalElement x) = variationPoint x
  -- SerializationTechnology
  -- ISignal
  -- ISignalGroup
  variationPoint (SystemSignalGroupElement x) = variationPoint x
  -- ISignalPdu
  -- NmPdu
  -- NPdu
  -- XcpPdu
  -- DcmlPdu
  -- J1939DcmIPdu
  -- UserDefinedPdu
  -- UserDefinedIPdu
  -- ISignalIPduGroup
  -- PdurIPduGroup
  -- EndToEndProtectionSet
  -- MultiplexedIPdu
  -- FlexrayFrame
  -- LinFrame
  -- LinUnconditionalFrame
  -- LinSporadicFrame
  -- LinEventTriggeredFrame
  -- CanFrame
  -- SoAdRoutingGroup
  -- EthernetFrame
  -- NPdu
  -- FlexrayTpConfig
  -- CanTpConfig
  -- LinTpConfig
  -- J1939TpConfig
  -- NmConfig
  -- Gateway
  -- AliasNameSet
  -- ApplicationArrayDataType
  -- ApplicationPrimitiveDataType
  -- ApplicationRecordDataType
  -- CompuMethod
  -- FlexrayCluster
  -- FlexrayTpConfig
  -- NmPdu
  -- BlueprintMappingSet
  -- PostBuildVariantCriterion
  -- EvaluatedVariantSet
  -- PredefinedVariant
  -- SwSystemconstantValueSet
  -- BuildActionManifest
  -- AclPermission
  -- AclOperation
  -- AclRole
  -- LifeCycleStateDefinitionGroup
  -- LifeCycleInfoSet
  -- Collection
  variationPoint (ParameterSwComponentTypeElement x) = variationPoint x
  variationPoint (CompositionSwComponentTypeElement x) = variationPoint x
  variationPoint (ApplicationSwComponentTypeElement x) = variationPoint x
  variationPoint (EcuAbstractionSwComponentTypeElement x) = variationPoint x
  variationPoint (ComplexDeviceDriverSwComponentTypeElement x) = variationPoint x
  variationPoint (ServiceSwComponentTypeElement x) = variationPoint x
  variationPoint (ServiceProxySwComponentTypeElement x) = variationPoint x
  variationPoint (NvBlockSwComponentTypeElement x) = variationPoint x
  variationPoint (SensorActuatorSwComponentTypeElement x) = variationPoint x
  -- BswModuleDescription
  -- BswModuleEntry
  -- Documentation
  -- Unit
  variationPoint (ClientServerInterfaceElement x) = variationPoint x
  variationPoint (TriggerInterfaceElement x) = variationPoint x
  variationPoint (ParameterInterfaceElement x) = variationPoint x
  variationPoint (SenderReceiverInterfaceElement x) = variationPoint x
  variationPoint (NvDataInterfaceElement x) = variationPoint x
  variationPoint (ModeSwitchInterfaceElement x) = variationPoint x
  -- ImplementationDataType
  -- ModeDeclarationGroup
  -- ModeDeclarationMappingSet
  variationPoint (PortInterfaceMappingSetElement x) = variationPoint x
  -- EndToEndProtectionSet
  variationPoint (DataTypeMappingSetElement x) = variationPoint x
  -- PhysicalDimension
  -- PhysicalDimensionMappingSet
  variationPoint (UnitGroupElement x) = variationPoint x
  -- DataConstr
  variationPoint (SwAddrMethodElement x) = variationPoint x
  -- SwRecordLayout
  -- InterpolationRoutineMappingSet
  -- ConstantSpecification
  variationPoint (ConstantSpecificationMappingSetElement x) = variationPoint x
  variationPoint (CalibrationParameterValueSetElement x) = variationPoint x
  -- RapidPrototypingScenario
  -- BswImplementation
  -- EcucValueCollection
  -- HwType
  -- ISignalIPdu
  variationPoint (SwcBswMappingElement x) = variationPoint x
  variationPoint (SwComponentPrototypeElement x) = variationPoint x
  variationPoint (EcuMappingElement x) = variationPoint x
  variationPoint (SwcToImplMappingElement x) = variationPoint x
  variationPoint (SwcToEcuMappingElement x) = variationPoint x
  variationPoint (ConsistencyNeedsElement x) = variationPoint x
  variationPoint (PortGroupElement x) = variationPoint x
  variationPoint (NvBlockDescriptorElement x) = variationPoint x
  variationPoint (ClientServerOperationElement x) = variationPoint x
  variationPoint (ParameterDataPrototypeElement x) = variationPoint x
  variationPoint (ExclusiveAreaElement x) = variationPoint x
  variationPoint (ServiceDependencyElement x) = variationPoint x
  variationPoint (RunnableEntityElement x) = variationPoint x
  variationPoint (PerInstanceMemoryElement x) = variationPoint x
  variationPoint (RTEEventElement x) = variationPoint x
  variationPoint (VariableDataPrototypeElement x) = variationPoint x
  variationPoint (ExclusiveAreaNestingOrderElement x) = variationPoint x
  variationPoint (ArgumentDataPrototypeElement x) = variationPoint x
  variationPoint (ServerCallPointElement x) = variationPoint x
  variationPoint (ParameterAccessElement x) = variationPoint x
  variationPoint (ModeSwitchPointElement x) = variationPoint x
  variationPoint (InternalTriggeringPointElement x) = variationPoint x
  variationPoint (VariableAccessElement x) = variationPoint x
  variationPoint (AsynchronousServerCallResultPointElement x) = variationPoint x
  variationPoint (PortInterfaceMappingElement x) = variationPoint x
  -- variationPoint x = error ("HasVariationPoint Element not implemented on: " ++ (show x))

instance HasSubElements Element where
  aggregates (PortPrototypeElement x) = aggregates x
  aggregates (SwConnectorElement x) = aggregates x
  aggregates (RootSwCompositionPrototypeElement x) = aggregates x
  aggregates (SystemMappingElement x) = aggregates x
  aggregates (FibexElementElement x) = aggregates x
  aggregates (SwcInternalBehaviorElement x) = aggregates x
  -- packageable elements:
  aggregates (SystemElement x) = aggregates x
  aggregates (SwSystemconstElement x) = aggregates x
  aggregates (EcuInstanceElement x) = aggregates x
  aggregates (FlatMapElement x) = aggregates x
  aggregates (HwElementElement x) = aggregates x
  -- SwBaseType
  -- SwRecordLayout
  -- SwSystemconstantValueSet
  aggregates (SwcImplementationElement x) = aggregates x
  -- ViewMapSet
  -- CanCluster
  -- TtcanCluster
  -- J1939Cluster
  -- FlexrayCluster
  -- LinCluster
  -- EthernetCluster
  -- CouplingElement
  -- UserDefinedCluster
  aggregates (SystemSignalElement x) = aggregates x
  -- SerializationTechnology
  -- ISignal
  -- ISignalGroup
  aggregates (SystemSignalGroupElement x) = aggregates x
  -- ISignalPdu
  -- NmPdu
  -- NPdu
  -- XcpPdu
  -- DcmlPdu
  -- J1939DcmIPdu
  -- UserDefinedPdu
  -- UserDefinedIPdu
  -- ISignalIPduGroup
  -- PdurIPduGroup
  -- EndToEndProtectionSet
  -- MultiplexedIPdu
  -- FlexrayFrame
  -- LinFrame
  -- LinUnconditionalFrame
  -- LinSporadicFrame
  -- LinEventTriggeredFrame
  -- CanFrame
  -- SoAdRoutingGroup
  -- EthernetFrame
  -- NPdu
  -- FlexrayTpConfig
  -- CanTpConfig
  -- LinTpConfig
  -- J1939TpConfig
  -- NmConfig
  -- Gateway
  -- AliasNameSet
  -- ApplicationArrayDataType
  -- ApplicationPrimitiveDataType
  -- ApplicationRecordDataType
  -- CompuMethod
  -- FlexrayCluster
  -- FlexrayTpConfig
  -- NmPdu
  -- BlueprintMappingSet
  -- PostBuildVariantCriterion
  -- EvaluatedVariantSet
  -- PredefinedVariant
  -- SwSystemconstantValueSet
  -- BuildActionManifest
  -- AclPermission
  -- AclOperation
  -- AclRole
  -- LifeCycleStateDefinitionGroup
  -- LifeCycleInfoSet
  -- Collection
  aggregates (ParameterSwComponentTypeElement x) = aggregates x
  aggregates (CompositionSwComponentTypeElement x) = aggregates x
  aggregates (ApplicationSwComponentTypeElement x) = aggregates x
  aggregates (EcuAbstractionSwComponentTypeElement x) = aggregates x
  aggregates (ComplexDeviceDriverSwComponentTypeElement x) = aggregates x
  aggregates (ServiceSwComponentTypeElement x) = aggregates x
  aggregates (ServiceProxySwComponentTypeElement x) = aggregates x
  aggregates (NvBlockSwComponentTypeElement x) = aggregates x
  aggregates (SensorActuatorSwComponentTypeElement x) = aggregates x
  -- BswModuleDescription
  -- BswModuleEntry
  -- Documentation
  -- Unit
  aggregates (ClientServerInterfaceElement x) = aggregates x
  aggregates (TriggerInterfaceElement x) = aggregates x
  aggregates (ParameterInterfaceElement x) = aggregates x
  aggregates (SenderReceiverInterfaceElement x) = aggregates x
  aggregates (NvDataInterfaceElement x) = aggregates x
  aggregates (ModeSwitchInterfaceElement x) = aggregates x
  -- ImplementationDataType
  -- ModeDeclarationGroup
  -- ModeDeclarationMappingSet
  aggregates (PortInterfaceMappingSetElement x) = aggregates x
  -- EndToEndProtectionSet
  aggregates (DataTypeMappingSetElement x) = aggregates x
  -- PhysicalDimension
  -- PhysicalDimensionMappingSet
  aggregates (UnitGroupElement x) = aggregates x
  -- DataConstr
  aggregates (SwAddrMethodElement x) = aggregates x
  -- SwRecordLayout
  -- InterpolationRoutineMappingSet
  -- ConstantSpecification
  aggregates (ConstantSpecificationMappingSetElement x) = aggregates x
  aggregates (CalibrationParameterValueSetElement x) = aggregates x
  -- RapidPrototypingScenario
  -- BswImplementation
  -- EcucValueCollection
  -- HwType
  -- ISignalIPdu
  aggregates (SwcBswMappingElement x) = aggregates x
  aggregates (SwComponentPrototypeElement x) = aggregates x
  aggregates (EcuMappingElement x) = aggregates x
  aggregates (SwcToImplMappingElement x) = aggregates x
  aggregates (SwcToEcuMappingElement x) = aggregates x
  aggregates (ConsistencyNeedsElement x) = aggregates x
  aggregates (PortGroupElement x) = aggregates x
  aggregates (NvBlockDescriptorElement x) = aggregates x
  aggregates (ClientServerOperationElement x) = aggregates x
  aggregates (ParameterDataPrototypeElement x) = aggregates x
  aggregates (ExclusiveAreaElement x) = aggregates x
  aggregates (ServiceDependencyElement x) = aggregates x
  aggregates (RunnableEntityElement x) = aggregates x
  aggregates (PerInstanceMemoryElement x) = aggregates x
  aggregates (RTEEventElement x) = aggregates x
  aggregates (VariableDataPrototypeElement x) = aggregates x
  aggregates (ExclusiveAreaNestingOrderElement x) = aggregates x
  aggregates (ArgumentDataPrototypeElement x) = aggregates x
  aggregates (ServerCallPointElement x) = aggregates x
  aggregates (ParameterAccessElement x) = aggregates x
  aggregates (ModeSwitchPointElement x) = aggregates x
  aggregates (InternalTriggeringPointElement x) = aggregates x
  aggregates (VariableAccessElement x) = aggregates x
  aggregates (AsynchronousServerCallResultPointElement x) = aggregates x
  aggregates (PortInterfaceMappingElement x) = aggregates x

  references (PortPrototypeElement x) = references x
  references (SwConnectorElement x) = references x
  references (RootSwCompositionPrototypeElement x) = references x
  references (SystemMappingElement x) = references x
  references (FibexElementElement x) = references x
  references (SwcInternalBehaviorElement x) = references x
  -- packageable elements:
  references (SystemElement x) = references x
  references (SwSystemconstElement x) = references x
  references (EcuInstanceElement x) = references x
  references (FlatMapElement x) = references x
  references (HwElementElement x) = references x
  -- SwBaseType
  -- SwRecordLayout
  -- SwSystemconstantValueSet
  references (SwcImplementationElement x) = references x
  -- ViewMapSet
  -- CanCluster
  -- TtcanCluster
  -- J1939Cluster
  -- FlexrayCluster
  -- LinCluster
  -- EthernetCluster
  -- CouplingElement
  -- UserDefinedCluster
  references (SystemSignalElement x) = references x
  -- SerializationTechnology
  -- ISignal
  -- ISignalGroup
  references (SystemSignalGroupElement x) = references x
  -- ISignalPdu
  -- NmPdu
  -- NPdu
  -- XcpPdu
  -- DcmlPdu
  -- J1939DcmIPdu
  -- UserDefinedPdu
  -- UserDefinedIPdu
  -- ISignalIPduGroup
  -- PdurIPduGroup
  -- EndToEndProtectionSet
  -- MultiplexedIPdu
  -- FlexrayFrame
  -- LinFrame
  -- LinUnconditionalFrame
  -- LinSporadicFrame
  -- LinEventTriggeredFrame
  -- CanFrame
  -- SoAdRoutingGroup
  -- EthernetFrame
  -- NPdu
  -- FlexrayTpConfig
  -- CanTpConfig
  -- LinTpConfig
  -- J1939TpConfig
  -- NmConfig
  -- Gateway
  -- AliasNameSet
  -- ApplicationArrayDataType
  -- ApplicationPrimitiveDataType
  -- ApplicationRecordDataType
  -- CompuMethod
  -- FlexrayCluster
  -- FlexrayTpConfig
  -- NmPdu
  -- BlueprintMappingSet
  -- PostBuildVariantCriterion
  -- EvaluatedVariantSet
  -- PredefinedVariant
  -- SwSystemconstantValueSet
  -- BuildActionManifest
  -- AclPermission
  -- AclOperation
  -- AclRole
  -- LifeCycleStateDefinitionGroup
  -- LifeCycleInfoSet
  -- Collection
  references (ParameterSwComponentTypeElement x) = references x
  references (CompositionSwComponentTypeElement x) = references x
  references (ApplicationSwComponentTypeElement x) = references x
  references (EcuAbstractionSwComponentTypeElement x) = references x
  references (ComplexDeviceDriverSwComponentTypeElement x) = references x
  references (ServiceSwComponentTypeElement x) = references x
  references (ServiceProxySwComponentTypeElement x) = references x
  references (NvBlockSwComponentTypeElement x) = references x
  references (SensorActuatorSwComponentTypeElement x) = references x
  -- BswModuleDescription
  -- BswModuleEntry
  -- Documentation
  -- Unit
  references (ClientServerInterfaceElement x) = references x
  references (TriggerInterfaceElement x) = references x
  references (ParameterInterfaceElement x) = references x
  references (SenderReceiverInterfaceElement x) = references x
  references (NvDataInterfaceElement x) = references x
  references (ModeSwitchInterfaceElement x) = references x
  -- ImplementationDataType
  -- ModeDeclarationGroup
  -- ModeDeclarationMappingSet
  references (PortInterfaceMappingSetElement x) = references x
  -- EndToEndProtectionSet
  references (DataTypeMappingSetElement x) = references x
  -- PhysicalDimension
  -- PhysicalDimensionMappingSet
  references (UnitGroupElement x) = references x
  -- DataConstr
  references (SwAddrMethodElement x) = references x
  -- SwRecordLayout
  -- InterpolationRoutineMappingSet
  -- ConstantSpecification
  references (ConstantSpecificationMappingSetElement x) = references x
  references (CalibrationParameterValueSetElement x) = references x
  -- RapidPrototypingScenario
  -- BswImplementation
  -- EcucValueCollection
  -- HwType
  -- ISignalIPdu
  references (SwcBswMappingElement x) = references x
  references (SwComponentPrototypeElement x) = references x
  references (EcuMappingElement x) = references x
  references (SwcToImplMappingElement x) = references x
  references (SwcToEcuMappingElement x) = references x
  references (ConsistencyNeedsElement x) = references x
  references (PortGroupElement x) = references x
  references (NvBlockDescriptorElement x) = references x
  references (ClientServerOperationElement x) = references x
  references (ParameterDataPrototypeElement x) = references x
  references (ExclusiveAreaElement x) = references x
  references (ServiceDependencyElement x) = references x
  references (RunnableEntityElement x) = references x
  references (PerInstanceMemoryElement x) = references x
  references (RTEEventElement x) = references x
  references (VariableDataPrototypeElement x) = references x
  references (ExclusiveAreaNestingOrderElement x) = references x
  references (ArgumentDataPrototypeElement x) = references x
  references (ServerCallPointElement x) = references x
  references (ParameterAccessElement x) = references x
  references (ModeSwitchPointElement x) = references x
  references (InternalTriggeringPointElement x) = references x
  references (VariableAccessElement x) = references x
  references (AsynchronousServerCallResultPointElement x) = references x
  references (PortInterfaceMappingElement x) = references x

  instantiates (PortPrototypeElement x) = instantiates x
  instantiates (SwConnectorElement x) = instantiates x
  instantiates (RootSwCompositionPrototypeElement x) = instantiates x
  instantiates (SystemMappingElement x) = instantiates x
  instantiates (FibexElementElement x) = instantiates x
  instantiates (SwcInternalBehaviorElement x) = instantiates x
  -- packageable elements:
  instantiates (SystemElement x) = instantiates x
  instantiates (SwSystemconstElement x) = instantiates x
  instantiates (EcuInstanceElement x) = instantiates x
  instantiates (FlatMapElement x) = instantiates x
  instantiates (HwElementElement x) = instantiates x
  -- SwBaseType
  -- SwRecordLayout
  -- SwSystemconstantValueSet
  instantiates (SwcImplementationElement x) = instantiates x
  -- ViewMapSet
  -- CanCluster
  -- TtcanCluster
  -- J1939Cluster
  -- FlexrayCluster
  -- LinCluster
  -- EthernetCluster
  -- CouplingElement
  -- UserDefinedCluster
  instantiates (SystemSignalElement x) = instantiates x
  -- SerializationTechnology
  -- ISignal
  -- ISignalGroup
  instantiates (SystemSignalGroupElement x) = instantiates x
  -- ISignalPdu
  -- NmPdu
  -- NPdu
  -- XcpPdu
  -- DcmlPdu
  -- J1939DcmIPdu
  -- UserDefinedPdu
  -- UserDefinedIPdu
  -- ISignalIPduGroup
  -- PdurIPduGroup
  -- EndToEndProtectionSet
  -- MultiplexedIPdu
  -- FlexrayFrame
  -- LinFrame
  -- LinUnconditionalFrame
  -- LinSporadicFrame
  -- LinEventTriggeredFrame
  -- CanFrame
  -- SoAdRoutingGroup
  -- EthernetFrame
  -- NPdu
  -- FlexrayTpConfig
  -- CanTpConfig
  -- LinTpConfig
  -- J1939TpConfig
  -- NmConfig
  -- Gateway
  -- AliasNameSet
  -- ApplicationArrayDataType
  -- ApplicationPrimitiveDataType
  -- ApplicationRecordDataType
  -- CompuMethod
  -- FlexrayCluster
  -- FlexrayTpConfig
  -- NmPdu
  -- BlueprintMappingSet
  -- PostBuildVariantCriterion
  -- EvaluatedVariantSet
  -- PredefinedVariant
  -- SwSystemconstantValueSet
  -- BuildActionManifest
  -- AclPermission
  -- AclOperation
  -- AclRole
  -- LifeCycleStateDefinitionGroup
  -- LifeCycleInfoSet
  -- Collection
  instantiates (ParameterSwComponentTypeElement x) = instantiates x
  instantiates (CompositionSwComponentTypeElement x) = instantiates x
  instantiates (ApplicationSwComponentTypeElement x) = instantiates x
  instantiates (EcuAbstractionSwComponentTypeElement x) = instantiates x
  instantiates (ComplexDeviceDriverSwComponentTypeElement x) = instantiates x
  instantiates (ServiceSwComponentTypeElement x) = instantiates x
  instantiates (ServiceProxySwComponentTypeElement x) = instantiates x
  instantiates (NvBlockSwComponentTypeElement x) = instantiates x
  instantiates (SensorActuatorSwComponentTypeElement x) = instantiates x
  -- BswModuleDescription
  -- BswModuleEntry
  -- Documentation
  -- Unit
  instantiates (ClientServerInterfaceElement x) = instantiates x
  instantiates (TriggerInterfaceElement x) = instantiates x
  instantiates (ParameterInterfaceElement x) = instantiates x
  instantiates (SenderReceiverInterfaceElement x) = instantiates x
  instantiates (NvDataInterfaceElement x) = instantiates x
  instantiates (ModeSwitchInterfaceElement x) = instantiates x
  -- ImplementationDataType
  -- ModeDeclarationGroup
  -- ModeDeclarationMappingSet
  instantiates (PortInterfaceMappingSetElement x) = instantiates x
  -- EndToEndProtectionSet
  instantiates (DataTypeMappingSetElement x) = instantiates x
  -- PhysicalDimension
  -- PhysicalDimensionMappingSet
  instantiates (UnitGroupElement x) = instantiates x
  -- DataConstr
  instantiates (SwAddrMethodElement x) = instantiates x
  -- SwRecordLayout
  -- InterpolationRoutineMappingSet
  -- ConstantSpecification
  instantiates (ConstantSpecificationMappingSetElement x) = instantiates x
  instantiates (CalibrationParameterValueSetElement x) = instantiates x
  -- RapidPrototypingScenario
  -- BswImplementation
  -- EcucValueCollection
  -- HwType
  -- ISignalIPdu
  instantiates (SwcBswMappingElement x) = instantiates x
  instantiates (SwComponentPrototypeElement x) = instantiates x
  instantiates (EcuMappingElement x) = instantiates x
  instantiates (SwcToImplMappingElement x) = instantiates x
  instantiates (SwcToEcuMappingElement x) = instantiates x
  instantiates (ConsistencyNeedsElement x) = instantiates x
  instantiates (PortGroupElement x) = instantiates x
  instantiates (NvBlockDescriptorElement x) = instantiates x
  instantiates (ClientServerOperationElement x) = instantiates x
  instantiates (ParameterDataPrototypeElement x) = instantiates x
  instantiates (ExclusiveAreaElement x) = instantiates x
  instantiates (ServiceDependencyElement x) = instantiates x
  instantiates (RunnableEntityElement x) = instantiates x
  instantiates (PerInstanceMemoryElement x) = instantiates x
  instantiates (RTEEventElement x) = instantiates x
  instantiates (VariableDataPrototypeElement x) = instantiates x
  instantiates (ExclusiveAreaNestingOrderElement x) = instantiates x
  instantiates (ArgumentDataPrototypeElement x) = instantiates x
  instantiates (ServerCallPointElement x) = instantiates x
  instantiates (ParameterAccessElement x) = instantiates x
  instantiates (ModeSwitchPointElement x) = instantiates x
  instantiates (InternalTriggeringPointElement x) = instantiates x
  instantiates (VariableAccessElement x) = instantiates x
  instantiates (AsynchronousServerCallResultPointElement x) = instantiates x
  instantiates (PortInterfaceMappingElement x) = instantiates x

instance Referable Element where
  shortName (ModeDeclarationGroupPrototypeElement s) = shortName s
  shortName (TriggerElement s) = shortName s
  shortName (SystemElement s) = shortName s
  shortName (SwSystemconstElement s) = shortName s
  shortName (SystemMappingElement s) = shortName s
  shortName (RootSwCompositionPrototypeElement s) = shortName s
  shortName (EcuInstanceElement x) = shortName x
  shortName (FlatMapElement x) = shortName x
  shortName (HwElementElement x) = shortName x
  shortName (SwcImplementationElement x) = shortName x
  shortName (ParameterSwComponentTypeElement x) = shortName x
  shortName (CompositionSwComponentTypeElement x) = shortName x
  shortName (ApplicationSwComponentTypeElement x) = shortName x
  shortName (EcuAbstractionSwComponentTypeElement x) = shortName x
  shortName (ComplexDeviceDriverSwComponentTypeElement x) = shortName x
  shortName (ServiceSwComponentTypeElement x) = shortName x
  shortName (ServiceProxySwComponentTypeElement x) = shortName x
  shortName (NvBlockSwComponentTypeElement x) = shortName x
  shortName (SensorActuatorSwComponentTypeElement x) = shortName x
  shortName (ClientServerInterfaceElement x) = shortName x
  shortName (TriggerInterfaceElement x) = shortName x
  shortName (ParameterInterfaceElement x) = shortName x
  shortName (SenderReceiverInterfaceElement x) = shortName x
  shortName (NvDataInterfaceElement x) = shortName x
  shortName (ModeSwitchInterfaceElement x) = shortName x
  shortName (DataTypeMappingSetElement x) = shortName x
  shortName (UnitGroupElement x) = shortName x
  shortName (SwAddrMethodElement x) = shortName x
  shortName (ConstantSpecificationMappingSetElement x) = shortName x
  shortName (CalibrationParameterValueSetElement x) = shortName x
  shortName (SwcBswMappingElement x) = shortName x
  shortName (EcuMappingElement x) = shortName x
  shortName (SwcToImplMappingElement x) = shortName x
  shortName (SwcToEcuMappingElement x) = shortName x
  shortName (ConsistencyNeedsElement x) = shortName x
  shortName (PortGroupElement x) = shortName x
  shortName (NvBlockDescriptorElement x) = shortName x
  shortName (ClientServerOperationElement x) = shortName x
  shortName (ParameterDataPrototypeElement x) = shortName x
  shortName (ExclusiveAreaElement x) = shortName x
  shortName (ServiceDependencyElement x) = shortName x
  shortName (RunnableEntityElement x) = shortName x
  shortName (PerInstanceMemoryElement x) = shortName x
  shortName (RTEEventElement x) = shortName x
  shortName (VariableDataPrototypeElement x) = shortName x
  shortName (ExclusiveAreaNestingOrderElement x) = shortName x
  shortName (ArgumentDataPrototypeElement x) = shortName x
  shortName (ServerCallPointElement x) = shortName x
  shortName (ParameterAccessElement x) = shortName x
  shortName (ModeSwitchPointElement x) = shortName x
  shortName (InternalTriggeringPointElement x) = shortName x
  shortName (VariableAccessElement x) = shortName x
  shortName (AsynchronousServerCallResultPointElement x) = shortName x
  shortName (PortInterfaceMappingElement x) = shortName x
  shortName (SwConnectorElement x) = shortName x
  shortName (SwComponentPrototypeElement x) = shortName x
  shortName (PortPrototypeElement x) = shortName x
  shortName (PortInterfaceMappingSetElement x) = shortName x
  shortName x = error ("Referable Element not implemented on " ++ (show x))

instance HasVariationPoint PackageableElement where
  variationPoint (SystemPElement v) = variationPoint v
  variationPoint (SwSystemconstPElement v) = variationPoint v
  variationPoint (EcuInstancePElement v) = variationPoint v
  variationPoint (FlatMapPElement v) = variationPoint v
  variationPoint (HwElementPElement v) = variationPoint v
  variationPoint (SwcImplementationPElement v) = variationPoint v
  variationPoint (ParameterSwComponentTypePElement v) = variationPoint v
  variationPoint (CompositionSwComponentTypePElement v) = variationPoint v
  variationPoint (ApplicationSwComponentTypePElement v) = variationPoint v
  variationPoint (EcuAbstractionSwComponentTypePElement v) = variationPoint v
  variationPoint (ComplexDeviceDriverSwComponentTypePElement v) = variationPoint v
  variationPoint (ServiceSwComponentTypePElement v) = variationPoint v
  variationPoint (ServiceProxySwComponentTypePElement v) = variationPoint v
  variationPoint (NvBlockSwComponentTypePElement v) = variationPoint v
  variationPoint (SensorActuatorSwComponentTypePElement v) = variationPoint v
  variationPoint (ClientServerInterfacePElement v) = variationPoint v
  variationPoint (TriggerInterfacePElement v) = variationPoint v
  variationPoint (ParameterInterfacePElement v) = variationPoint v
  variationPoint (SenderReceiverInterfacePElement v) = variationPoint v
  variationPoint (NvDataInterfacePElement v) = variationPoint v
  variationPoint (ModeSwitchInterfacePElement v) = variationPoint v
  variationPoint (DataTypeMappingSetPElement v) = variationPoint v
  variationPoint (UnitGroupPElement v) = variationPoint v
  variationPoint (SwAddrMethodPElement v) = variationPoint v
  variationPoint (ConstantSpecificationMappingSetPElement v) = variationPoint v
  variationPoint (CalibrationParameterValueSetPElement v) = variationPoint v
  variationPoint (SwcBswMappingPElement v) = variationPoint v
  variationPoint (PortInterfaceMappingSetPElement v) = variationPoint v
  -- variationPoint x = error ("HasVariationPoint PackageableElement not implemented on " ++ (show (toElement x)))

instance HasVariableSubElements PackageableElement where
  variableSubElements (ParameterSwComponentTypePElement s) = variableSubElements s
  -- FIXME

data SwSystemconst where
  SwSystemconst :: { 
        shortName_0                    :: String,
        constSwDataDefProps            :: Maybe SwDataDefProps,
        swSystemconst_variationPoint   :: Maybe VariationPoint
    } -> SwSystemconst

deriving instance Show SwSystemconst

instance Referable SwSystemconst where
        shortName                   = shortName_0

instance HasVariationPoint SwSystemconst where
  variationPoint = swSystemconst_variationPoint

instance HasSubElements SwSystemconst where
  aggregates _ = []
  references _ = []
  instantiates _ = []

-----------------------------------------------------------------
-- System
-----------------------------------------------------------------

data System where
  System :: {
        ecuExtractVersion               :: Maybe String,
        fibexElement                    :: Var (Ref FibexElement),      -- atpVariation, 0..*
        system_mapping                  :: Var SystemMapping, -- atpVariation, atpSplitable, 0..*
        pncVectorLength                 :: Maybe Int,
        pncVectorOffset                 :: Maybe Int,
        rootSoftwareComposition         :: Var RootSwCompositionPrototype, -- atpVariation, 0..1
        systemDocumentation             :: [String], -- TODO atpVariation, atpSplitable, 0..*
        systemVersion                   :: Maybe String, -- pureMM.minOccurs="1"
        system_shortName                :: String,
        system_variationPoint           :: Maybe VariationPoint
    } -> System

deriving instance Eq System
deriving instance Show System

instance Referable System where
  shortName = system_shortName

instance HasVariableSubElements System where
  variableSubElements se =
    let mps = system_mapping se
        rscs = rootSoftwareComposition se
    in
      (map SystemMappingElement mps) ++ (map RootSwCompositionPrototypeElement rscs)

instance HasSubElements System where
  aggregates v = (map SystemMappingElement (system_mapping v)) ++ (map RootSwCompositionPrototypeElement (rootSoftwareComposition v))
  references v = (map AnyRef (fibexElement v))
  instantiates v = []

instance HasVariationPoint System where
  variationPoint = system_variationPoint

data RootSwCompositionPrototype where
   RootSwCompositionPrototype :: {
        rootSwCompositionPrototype_shortName :: String,
        calibrationParameterValueSet    :: [Ref CalibrationParameterValueSet],
        flatMap                         :: Maybe (Ref FlatMap),
        softwareComposition             :: Ref CompositionSwComponentType, -- pureMM.minOccurs="1"
        rootSwCompositionPrototype_variationPoint :: Maybe VariationPoint
    } -> RootSwCompositionPrototype

deriving instance Show RootSwCompositionPrototype
deriving instance Eq RootSwCompositionPrototype

data CompositionSwComponentType_SubtypesEnum = CompositionSwComponentType_SubtypesEnum

instance Referable RootSwCompositionPrototype where
  shortName = rootSwCompositionPrototype_shortName

instance HasVariationPoint RootSwCompositionPrototype where
  variationPoint = rootSwCompositionPrototype_variationPoint

instance HasSubElements RootSwCompositionPrototype where
  aggregates r = []
  references r = (map AnyRef (calibrationParameterValueSet r)) ++ (map AnyRef (maybeToList (flatMap r))) ++ [AnyRef (softwareComposition r)]
  instantiates r = []

-----------------------------------------------------------------
-- SystemMapping
-----------------------------------------------------------------

data SystemMapping where
  SystemMapping :: {
        systemMapping_shortName         :: String,
        dataMapping                     :: Var DataMapping, -- atpVariation, 0..*
        ecuResourceMapping              :: Var EcuMapping, -- atpVariation, 0..*
        systemMapping_mappingConstraint :: Var MappingConstraint, -- atpVariation, 0..*
        pncMapping                      :: Var PncMapping, -- atpVariation, 0..*
        resourceEstimation              :: Var EcuResourceEstimation, -- atpVariation, 0..*
        signalPathConstraint            :: Var SignalPathConstraint, -- atpVariation, 0..*
        swImplMapping                   :: Var SwcToImplMapping, -- atpVariation, 0..*
        swMapping                       :: Var SwcToEcuMapping, -- atpVariation, 0..*
        systemMapping_variationPoint    :: Maybe VariationPoint
    } -> SystemMapping

deriving instance Show SystemMapping
deriving instance Eq SystemMapping

instance Referable SystemMapping where
  shortName = systemMapping_shortName

instance ToElement SystemMapping where
  toElement = SystemMappingElement

instance HasVariationPoint SystemMapping where
  variationPoint = systemMapping_variationPoint

instance HasSubElements SystemMapping where
  aggregates sm = -- not referable: (map toElement (dataMapping sm)) ++
                  (map toElement (ecuResourceMapping sm)) ++
                  -- not referable: (map toElement (systemMapping_mappingConstraint sm)) ++
                  -- not referable: (map toElement (pncMapping sm)) ++
                  -- not referable: (map toElement (resourceEstimation sm)) ++
                  -- not referable: (map toElement (signalPathConstraint sm)) ++
                  (map toElement (swImplMapping sm)) ++
                  (map toElement (swMapping sm))
  references sm = []
  instantiates sm = []

data MappingConstraint_ where
  MappingConstraint :: {
        introduction                     :: Maybe String,
        mappingConstraint_variationPoint :: Maybe VariationPoint
  } -> MappingConstraint_

deriving instance Show MappingConstraint_
deriving instance Eq MappingConstraint_

data MappingConstraint =
    ComponentClustering {
        mappingConstraint               :: MappingConstraint_,
        clusteredComponent              :: [IRef SwComponentPrototype]
    } |
    ComponentSeparation {
        mappingConstraint               :: MappingConstraint_,
        separatedComponent              :: [IRef SwComponentPrototype] -- 0..2
    } |
    SwcToEcuMappingConstraint {
        mappingConstraint               :: MappingConstraint_,
        swcToEcuMappingConstraintcomponent    :: IRef SwComponentPrototype,
        swcToEcuMappingConstraint_ecuInstance :: [Ref EcuInstance],
        swcToEcuMappingConstraintType   :: SwcToEcuMappingConstraintType
    }

deriving instance Show MappingConstraint
deriving instance Eq MappingConstraint

data SwcToEcuMappingConstraintType      = Dedicated | Exclusive deriving (Show, Eq)

data SwcToImplMapping where
    SwcToImplMapping :: {
        swcToImplMapping_shortName       :: String,
        swcToImplMapping_component       :: [IRef SwComponentPrototype],
        componentImplementation          :: Ref SwcImplementation,
        swcToImplMapping_variationPoint  :: Maybe VariationPoint
    } -> SwcToImplMapping

deriving instance Eq SwcToImplMapping
deriving instance Show SwcToImplMapping

instance Referable SwcToImplMapping where
  shortName = swcToImplMapping_shortName

instance HasVariationPoint SwcToImplMapping where
  variationPoint = swcToImplMapping_variationPoint

instance ToElement SwcToImplMapping where
  toElement = SwcToImplMappingElement

instance HasSubElements SwcToImplMapping where
  aggregates _ = []
  references s = [AnyRef (componentImplementation s)]
  instantiates s = map AnyIRef (swcToImplMapping_component s)

data SwcToEcuMapping where
   SwcToEcuMapping :: {
        swcToEcuMapping_shortName       :: String,
        swcToEcuMapping_component       :: [IRef SwComponentPrototype],                                                                             
        controlledHwElement             :: Maybe (Ref HwElement),
        swcToEcuMapping_ecuInstance     :: Ref EcuInstance,
        swcToEcuMapping_partition       :: Maybe (Ref EcuPartition),
        processingUnit                  :: Maybe (Ref HwElement),
        swcToEcuMapping_variationPoint  :: Maybe VariationPoint
    } -> SwcToEcuMapping

deriving instance Eq SwcToEcuMapping
deriving instance Show SwcToEcuMapping

instance Referable SwcToEcuMapping where
  shortName = swcToEcuMapping_shortName

instance HasVariationPoint SwcToEcuMapping where
  variationPoint = swcToEcuMapping_variationPoint

instance ToElement SwcToEcuMapping where
  toElement = SwcToEcuMappingElement

instance HasSubElements SwcToEcuMapping where
  aggregates _ = []
  references s = (map AnyRef (maybeToList (controlledHwElement s))) ++
                 [AnyRef (swcToEcuMapping_ecuInstance s)] ++
                 (map AnyRef (maybeToList (swcToEcuMapping_partition s))) ++
                 (map AnyRef (maybeToList (processingUnit s)))
  instantiates s = map AnyIRef (swcToEcuMapping_component s)

-----------------------------------------------------------------
-- SwComponent
-----------------------------------------------------------------

-- v is Variable or []
-- (v, m) = ([], Maybe)
-- (v, m) = (Variable, Ignore)
-- resolveA :: Package -> Package Variable Ignore
-- resolveB :: Package -> (Ref -> [Variant (Element Ignore Ignore)])
-- FIXME: really tree

data SwComponentType_ where
  SwComponentType :: {
        swComponentType_shortName   :: String,
        swComponentType_variationPoint :: Maybe VariationPoint,
        consistencyNeeds            :: Var ConsistencyNeeds,  -- atpVariation, atpSplitable, 0..*
        port                        :: Var PortPrototype, -- atpVariation, atpSplitable, 0..*
        portGroup                   :: Var PortGroup, -- atpVariation, 0..*
        swComponentDocumentation    :: Var SwComponentDocumentation, -- atpVariation, atpSplitable, 0..1
        unitGroup                   :: [Ref UnitGroup]
    } -> SwComponentType_

deriving instance Show SwComponentType_

instance Referable SwComponentType_ where
        shortName                   = swComponentType_shortName

instance HasVariationPoint SwComponentType_ where
  variationPoint = swComponentType_variationPoint

instance HasSubElements SwComponentType_ where
  aggregates s = (map toElement (consistencyNeeds s)) ++
                 (map toElement (port s)) ++
                 (map toElement (portGroup s))
                 -- not referable: (map toElement (swComponentDocumentation s))
  references s = map AnyRef (unitGroup s)
  instantiates s = []

data AtomicSwComponentType_ where
    AtomicSwComponentType :: Show [SwcInternalBehavior] => { 
        atomicSwComponentType_swComponentType :: SwComponentType_,
        internalBehavior            :: Var SwcInternalBehavior, -- atpVariation, atpSplitable, 0..1
        symbolProps                 :: Maybe SymbolProps -- atpSplitable
    } -> AtomicSwComponentType_

deriving instance Show AtomicSwComponentType_

instance Referable AtomicSwComponentType_ where
        shortName                   = shortName . atomicSwComponentType_swComponentType

instance HasVariableSubElements AtomicSwComponentType_ where
  variableSubElements v = map SwcInternalBehaviorElement (internalBehavior v)

instance HasSubElements AtomicSwComponentType_ where
  aggregates a = (aggregates (atomicSwComponentType_swComponentType a)) ++
                 (map toElement (internalBehavior a))
  references a = references (atomicSwComponentType_swComponentType a)
  instantiates a = instantiates (atomicSwComponentType_swComponentType a)

data ParameterSwComponentType where
    ParameterSwComponentType :: {
        parameterSwComponentType_swComponentType :: SwComponentType_,
        constantMapping             :: [ConstantSpecificationMappingSet],
        parameterSwComponentType_dataTypeMapping :: [DataTypeMappingSet],
        parameterSwComponentType_instantiationDataDefProps :: Var InstantiationDataDefProps -- atpVariation, 0..*
    } -> ParameterSwComponentType

deriving instance Show ParameterSwComponentType

instance HasVariationPoint ParameterSwComponentType where
  variationPoint = variationPoint . parameterSwComponentType_swComponentType

instance HasVariableSubElements ParameterSwComponentType where
  variableSubElements psct = [] -- FIXME variable, but have no name! map parameterSwComponentType_instantiationDataDefProps psct

instance HasSubElements ParameterSwComponentType where
  aggregates p = (aggregates (parameterSwComponentType_swComponentType p)) ++
                 (map toElement (constantMapping p)) ++
                 (map toElement (parameterSwComponentType_dataTypeMapping p))
                 -- not referable: (map toElement (parameterSwComponentType_instantiationDataDefProps p))
  references p = references (parameterSwComponentType_swComponentType p)
  instantiates p = instantiates (parameterSwComponentType_swComponentType p)

instance Referable ParameterSwComponentType where
        shortName                   = shortName . parameterSwComponentType_swComponentType

data CompositionSwComponentType where
    CompositionSwComponentType :: {
        compositionSwComponentType_swComponentType      :: SwComponentType_,
        compositionSwComponentType_component            :: Var SwComponentPrototype, -- atpVariation, atpSplitable, 0..*
        connectors                  :: Var SwConnector, -- atpVariation, atpSplitable, 0..*
        compositionSwComponentType_constantValueMapping :: [Ref ConstantSpecificationMappingSet],
        compositionSwComponentType_dataTypeMapping      :: [Ref DataTypeMappingSet],
        instantiationRTEEventProps  :: Var InstantiationRTEEventProps -- atpVariation, atpSplitable, 0..*
    } -> CompositionSwComponentType

deriving instance Show CompositionSwComponentType

instance HasVariationPoint CompositionSwComponentType where
  variationPoint = variationPoint . compositionSwComponentType_swComponentType

instance HasSubElements CompositionSwComponentType where
  aggregates c = (aggregates (compositionSwComponentType_swComponentType c)) ++
                 (map toElement (compositionSwComponentType_component c)) ++
                 (map toElement (connectors c))
                 -- no referable: (map toElement (instantiationRTEEventProps c))
  references c = (references (compositionSwComponentType_swComponentType c)) ++
                 (map AnyRef (compositionSwComponentType_constantValueMapping c)) ++
                 (map AnyRef (compositionSwComponentType_dataTypeMapping c))
  instantiates c = instantiates (compositionSwComponentType_swComponentType c)

instance Referable CompositionSwComponentType where
        shortName                   = shortName . compositionSwComponentType_swComponentType
    
data ApplicationSwComponentType where
    ApplicationSwComponentType :: {
        applicationSwComponentType_atomicSwComponentType     :: AtomicSwComponentType_
    } -> ApplicationSwComponentType

deriving instance Show ApplicationSwComponentType

instance HasVariationPoint ApplicationSwComponentType where
  variationPoint = variationPoint . atomicSwComponentType_swComponentType . applicationSwComponentType_atomicSwComponentType

instance HasSubElements ApplicationSwComponentType where
  aggregates a = aggregates (applicationSwComponentType_atomicSwComponentType a)
  references a = references (applicationSwComponentType_atomicSwComponentType a)
  instantiates a = instantiates (applicationSwComponentType_atomicSwComponentType a)

instance Referable ApplicationSwComponentType where
        shortName                   = shortName . applicationSwComponentType_atomicSwComponentType

instance ToElement ApplicationSwComponentType where
  toElement = ApplicationSwComponentTypeElement

instance HasVariableSubElements ApplicationSwComponentType where
  variableSubElements = variableSubElements . applicationSwComponentType_atomicSwComponentType

data EcuAbstractionSwComponentType where
    EcuAbstractionSwComponentType :: {
        ecuAbstractionSwComponentType_atomicSwComponentType :: AtomicSwComponentType_,
        ecuAbstractionSwComponentType_hardwareElement :: [Ref HwDescriptionEntity]
        } -> EcuAbstractionSwComponentType

deriving instance Show EcuAbstractionSwComponentType

instance HasVariationPoint EcuAbstractionSwComponentType where
  variationPoint = variationPoint . atomicSwComponentType_swComponentType . ecuAbstractionSwComponentType_atomicSwComponentType

instance HasSubElements EcuAbstractionSwComponentType where
  aggregates e = aggregates (ecuAbstractionSwComponentType_atomicSwComponentType e)
  references e = references (ecuAbstractionSwComponentType_atomicSwComponentType e) ++
                 (map AnyRef (ecuAbstractionSwComponentType_hardwareElement e))
  instantiates e = instantiates (ecuAbstractionSwComponentType_atomicSwComponentType e)

instance Referable EcuAbstractionSwComponentType where
        shortName                   = shortName . ecuAbstractionSwComponentType_atomicSwComponentType

data ComplexDeviceDriverSwComponentType where
    ComplexDeviceDriverSwComponentType :: {
        complexDeviceDriverSwComponentType_atomicSwComponentType     :: AtomicSwComponentType_,
        complexDeviceDriverSwComponentType_hardwareElement           :: [Ref HwDescriptionEntity]
    } -> ComplexDeviceDriverSwComponentType

deriving instance Show ComplexDeviceDriverSwComponentType

instance HasVariationPoint ComplexDeviceDriverSwComponentType where
  variationPoint = variationPoint . atomicSwComponentType_swComponentType . complexDeviceDriverSwComponentType_atomicSwComponentType

instance HasSubElements ComplexDeviceDriverSwComponentType where
  aggregates c = aggregates (complexDeviceDriverSwComponentType_atomicSwComponentType c)
  references c = (references (complexDeviceDriverSwComponentType_atomicSwComponentType c)) ++
                 (map AnyRef (complexDeviceDriverSwComponentType_hardwareElement c))
  instantiates c = instantiates (complexDeviceDriverSwComponentType_atomicSwComponentType c)

instance Referable ComplexDeviceDriverSwComponentType where
        shortName                   = shortName . complexDeviceDriverSwComponentType_atomicSwComponentType

data ServiceSwComponentType where
    ServiceSwComponentType :: {
        serviceSwComponentType_atomicSwComponentType :: AtomicSwComponentType_
    } -> ServiceSwComponentType

deriving instance Show ServiceSwComponentType

instance HasVariationPoint ServiceSwComponentType where
  variationPoint = variationPoint . atomicSwComponentType_swComponentType . serviceSwComponentType_atomicSwComponentType

instance HasSubElements ServiceSwComponentType where
  aggregates s = aggregates (serviceSwComponentType_atomicSwComponentType s)
  references s = references (serviceSwComponentType_atomicSwComponentType s)
  instantiates s = instantiates (serviceSwComponentType_atomicSwComponentType s)

instance Referable ServiceSwComponentType where
        shortName                   = shortName . serviceSwComponentType_atomicSwComponentType

data ServiceProxySwComponentType where
    ServiceProxySwComponentType :: {
        serviceProxySwComponentType_atomicSwComponentType     :: AtomicSwComponentType_
    } -> ServiceProxySwComponentType

deriving instance Show ServiceProxySwComponentType

instance HasVariationPoint ServiceProxySwComponentType where
  variationPoint = variationPoint . atomicSwComponentType_swComponentType . serviceProxySwComponentType_atomicSwComponentType

instance HasSubElements ServiceProxySwComponentType where
  aggregates s = aggregates (serviceProxySwComponentType_atomicSwComponentType s)
  references s = references (serviceProxySwComponentType_atomicSwComponentType s)
  instantiates s = instantiates (serviceProxySwComponentType_atomicSwComponentType s)

instance Referable ServiceProxySwComponentType where
        shortName                   = shortName . serviceProxySwComponentType_atomicSwComponentType

data NvBlockSwComponentType where
    NvBlockSwComponentType :: {
        nvBlockSwComponentType_atomicSwComponentType :: AtomicSwComponentType_,
        nvBlockDescriptor           :: Var NvBlockDescriptor         -- aptSplitable, aptVariation, 0..*
    } -> NvBlockSwComponentType

deriving instance Show NvBlockSwComponentType

instance HasVariationPoint NvBlockSwComponentType where
  variationPoint = variationPoint . atomicSwComponentType_swComponentType . nvBlockSwComponentType_atomicSwComponentType

instance HasSubElements NvBlockSwComponentType where
  aggregates n = (aggregates (nvBlockSwComponentType_atomicSwComponentType n)) ++
                 (map toElement (nvBlockDescriptor n))
  references n = references (nvBlockSwComponentType_atomicSwComponentType n)
  instantiates n = instantiates (nvBlockSwComponentType_atomicSwComponentType n)

instance Referable NvBlockSwComponentType where
        shortName                   = shortName . nvBlockSwComponentType_atomicSwComponentType

data SensorActuatorSwComponentType where
    SensorActuatorSwComponentType :: {
        sensorActuatorSwComponentType_atomicSwComponentType :: AtomicSwComponentType_,
        sensorActuator              :: Maybe (Ref HwDescriptionEntity) -- pureMM.minOccurs = 1
    } -> SensorActuatorSwComponentType

deriving instance Show SensorActuatorSwComponentType

instance HasVariationPoint SensorActuatorSwComponentType where
  variationPoint = variationPoint . atomicSwComponentType_swComponentType . sensorActuatorSwComponentType_atomicSwComponentType

instance HasSubElements SensorActuatorSwComponentType where
  aggregates s = aggregates (sensorActuatorSwComponentType_atomicSwComponentType s)
  references s = (references (sensorActuatorSwComponentType_atomicSwComponentType s)) ++
                 (maybe [] (\v -> [AnyRef v]) (sensorActuator s))
  instantiates s = instantiates (sensorActuatorSwComponentType_atomicSwComponentType s)

instance Referable SensorActuatorSwComponentType where
        shortName                   = shortName . sensorActuatorSwComponentType_atomicSwComponentType


data SwComponentType =
    ParameterSwComponentTypeSwComponentType ParameterSwComponentType
  | CompositionSwComponentTypeSwComponentType CompositionSwComponentType
  | ApplicationSwComponentTypeSwComponentType ApplicationSwComponentType
  | EcuAbstractionSwComponentTypeSwComponentType EcuAbstractionSwComponentType
  | ComplexDeviceDriverSwComponentTypeSwComponentType ComplexDeviceDriverSwComponentType
  | ServiceSwComponentTypeSwComponentType ServiceSwComponentType
  | ServiceProxySwComponentTypeSwComponentType ServiceProxySwComponentType
  | NvBlockSwComponentTypeSwComponentType NvBlockSwComponentType
  | SensorActuatorSwComponentTypeSwComponentType SensorActuatorSwComponentType

deriving instance Show SwComponentType

instance Referable SwComponentType where
  shortName (ParameterSwComponentTypeSwComponentType v) = shortName v
  shortName (CompositionSwComponentTypeSwComponentType v) = shortName v
  shortName (ApplicationSwComponentTypeSwComponentType v) = shortName v
  shortName (EcuAbstractionSwComponentTypeSwComponentType v) = shortName v
  shortName (ComplexDeviceDriverSwComponentTypeSwComponentType v) = shortName v
  shortName (ServiceSwComponentTypeSwComponentType v) = shortName v
  shortName (ServiceProxySwComponentTypeSwComponentType v) = shortName v
  shortName (NvBlockSwComponentTypeSwComponentType v) = shortName v
  shortName (SensorActuatorSwComponentTypeSwComponentType v) = shortName v

data SwComponentPrototype where SwComponentPrototype :: {
        swComponentPrototype_shortName      :: String,
        swComponentPrototype_type           :: Ref SwComponentType,
        swComponentPrototype_variationPoint :: Maybe VariationPoint
    } -> SwComponentPrototype

deriving instance Show SwComponentPrototype

instance Referable SwComponentPrototype where
  shortName = swComponentPrototype_shortName

instance ToElement SwComponentPrototype where
  toElement = SwComponentPrototypeElement

instance HasVariationPoint SwComponentPrototype where
  variationPoint = swComponentPrototype_variationPoint

instance HasSubElements SwComponentPrototype where
  aggregates s = []
  references s = [AnyRef (swComponentPrototype_type s)]
  instantiates s = []

data NvBlockDescriptor where
  NvBlockDescriptor :: {
    nvBlockDescriptor_shortName :: String,
    nvBlockDescriptor_variationPoint :: Maybe VariationPoint
  } -> NvBlockDescriptor

deriving instance Show NvBlockDescriptor

instance Referable NvBlockDescriptor where
  shortName = nvBlockDescriptor_shortName

instance ToElement NvBlockDescriptor where
  toElement = NvBlockDescriptorElement

instance HasVariationPoint NvBlockDescriptor where
  variationPoint = nvBlockDescriptor_variationPoint

instance HasSubElements NvBlockDescriptor where
  aggregates _ = []
  references _ = []
  instantiates _ = []

type HwDescriptionEntity = ()

-----------------------------------------------------------------
-- PortPrototype
-----------------------------------------------------------------

data Ignore a = Ignored deriving (Show, Eq)

-- m is either Ignore or Id
data PortPrototype_ where
    PortPrototype :: {
        portPrototype_shortName         :: String,
        clientServerAnnotation          :: [ClientServerAnnotation],
        delegatedPortAnnotation         :: Maybe DelegatedPortAnnotation,
        ioHwAbstractionServerAnnotation :: [IoHwAbstractionServerAnnotation],
        modePortAnnotation              :: [ModePortAnnotation],
        nvDataPortAnnotation            :: [NvDataPortAnnotation],
        parameterPortAnnotation         :: [ParameterPortAnnotation],
        senderReceiverAnnotation        :: [SenderReceiverAnnotation],
        triggerPortAnnotation           :: [TriggerPortAnnotation],
        portPrototype_variationPoint    :: Maybe VariationPoint
    } -> PortPrototype_

deriving instance Eq PortPrototype_
deriving instance Show PortPrototype_

instance HasVariationPoint PortPrototype_ where
  variationPoint = portPrototype_variationPoint

data PortPrototype =
    RPortPrototype {
        portPrototype               :: PortPrototype_,
        requiredComSpec             :: [RPortComSpec],
        requiredInterface           :: Ref PortInterface
    } |
    PPortPrototype {
        portPrototype               :: PortPrototype_,
        providedComSpec             :: [PPortComSpec],
        providedInterface           :: Ref PortInterface
    } |
    PRPortPrototype {
        portPrototype               :: PortPrototype_,
        requiredComSpec             :: [RPortComSpec],
        providedRequiredInterface   :: Ref PortInterface,
        providedComSpec             :: [PPortComSpec]
    }
    deriving (Show, Eq)

instance Referable PortPrototype where
  shortName p = portPrototype_shortName (portPrototype p)

instance ToElement PortPrototype where
  toElement = PortPrototypeElement

instance HasVariationPoint PortPrototype where
  variationPoint = variationPoint . portPrototype

instance HasSubElements PortPrototype where
  aggregates pp = []
  references (pp@RPortPrototype {}) = [AnyRef (requiredInterface pp)]
  references (pp@PPortPrototype {}) = [AnyRef (providedInterface pp)]
  references (pp@PRPortPrototype {}) = [AnyRef (providedRequiredInterface pp)]
  instantiates pp = []

-----------------------------------------------------------------
-- PortInterface
-----------------------------------------------------------------

data PortInterface_ where
    PortInterface :: {
        portInterface_shortName     :: String,
        portInterface_variationPoint :: Maybe VariationPoint,
        isService                   :: Bool,
        serviceKind                 :: Maybe ServiceProviderEnum
    } -> PortInterface_

deriving instance Show PortInterface_
deriving instance Eq PortInterface_

instance HasSubElements PortInterface_ where
  aggregates _ = []
  references _ = []
  instantiates _ = []

instance HasVariationPoint PortInterface_ where
  variationPoint = portInterface_variationPoint

data ParameterInterface where
    ParameterInterface :: {
        parameterInterface_portInterface :: PortInterface_,
        parameter                   :: [ParameterDataPrototype] -- invariant, 1..*
    } -> ParameterInterface

deriving instance Show ParameterInterface
deriving instance Eq ParameterInterface

instance Referable ParameterInterface where
  shortName = portInterface_shortName . parameterInterface_portInterface
instance HasVariationPoint ParameterInterface where
  variationPoint = variationPoint . parameterInterface_portInterface

instance HasSubElements ParameterInterface where
  aggregates p = (aggregates (parameterInterface_portInterface p)) ++
                 (map toElement (parameter p))
  references p = references (parameterInterface_portInterface p)
  instantiates p = instantiates (parameterInterface_portInterface p)

data SenderReceiverInterface where
    SenderReceiverInterface :: {
        senderReceiverInterface_portInterface :: PortInterface_,
        dataElement                 :: [VariableDataPrototype], -- invariant, 1..*
        invalidationPolicy          :: [InvalidationPolicy] -- invariant, 0..*
    } -> SenderReceiverInterface

deriving instance Eq SenderReceiverInterface
deriving instance Show SenderReceiverInterface

instance Referable SenderReceiverInterface where
  shortName = portInterface_shortName . senderReceiverInterface_portInterface
instance HasVariationPoint SenderReceiverInterface where
  variationPoint = variationPoint . senderReceiverInterface_portInterface

instance HasSubElements SenderReceiverInterface where
  aggregates s = (aggregates (senderReceiverInterface_portInterface s)) ++
                 (map toElement (dataElement s))
  references s = references (senderReceiverInterface_portInterface s)
  instantiates s = instantiates (senderReceiverInterface_portInterface s)
                      

data NvDataInterface where
    NvDataInterface :: {
        nvDataInterface_portInterface :: PortInterface_,
        nvData                        :: [VariableDataPrototype] -- invariant, 1..*
    } -> NvDataInterface

deriving instance Eq NvDataInterface
deriving instance Show NvDataInterface

instance Referable NvDataInterface where
  shortName = portInterface_shortName . nvDataInterface_portInterface
instance HasVariationPoint NvDataInterface where
  variationPoint = variationPoint . nvDataInterface_portInterface

instance HasSubElements NvDataInterface where
  aggregates n = (aggregates (nvDataInterface_portInterface n)) ++
                 (map toElement (nvData n))
  references n = references (nvDataInterface_portInterface n)
  instantiates n = instantiates (nvDataInterface_portInterface n)


data ModeSwitchInterface where
    ModeSwitchInterface :: {
        modeSwitchInterface_portInterface :: PortInterface_,
        modeGroup                         :: ModeDeclarationGroupPrototype
    } -> ModeSwitchInterface

deriving instance Eq ModeSwitchInterface
deriving instance Show ModeSwitchInterface

instance Referable ModeSwitchInterface where
  shortName = portInterface_shortName . modeSwitchInterface_portInterface
instance HasVariationPoint ModeSwitchInterface where
  variationPoint = variationPoint . modeSwitchInterface_portInterface

instance HasSubElements ModeSwitchInterface where
  aggregates m = aggregates (modeSwitchInterface_portInterface m)
  references m = references (modeSwitchInterface_portInterface m)
  instantiates m = instantiates (modeSwitchInterface_portInterface m)

data TriggerInterface where
    TriggerInterface :: {
        triggerInterface_portInterface :: PortInterface_,
        triggerInterface_trigger       :: [Trigger]
    } -> TriggerInterface

deriving instance Eq TriggerInterface
deriving instance Show TriggerInterface

instance Referable TriggerInterface where
  shortName = portInterface_shortName . triggerInterface_portInterface
instance HasVariationPoint TriggerInterface where
  variationPoint = variationPoint . triggerInterface_portInterface

instance HasSubElements TriggerInterface where
  aggregates t = (aggregates (triggerInterface_portInterface t)) -- FIXME ++ (map toElement (triggerInterface_trigger t))
  references t = references (triggerInterface_portInterface t)
  instantiates t = instantiates (triggerInterface_portInterface t)

data ClientServerInterface where
    ClientServerInterface :: {
        clientServerInterface_portInterface :: PortInterface_,
        clientServerInterface_operation     :: Var ClientServerOperation, -- atpVariation, 1..*
        clientServerInterface_possibleError :: [ApplicationError]
    } -> ClientServerInterface

deriving instance Eq ClientServerInterface
deriving instance Show ClientServerInterface

instance Referable ClientServerInterface where
  shortName = portInterface_shortName . clientServerInterface_portInterface
instance HasVariationPoint ClientServerInterface where
  variationPoint = variationPoint . clientServerInterface_portInterface

instance HasSubElements ClientServerInterface where
  aggregates c = (aggregates (clientServerInterface_portInterface c)) ++
                 (map toElement (clientServerInterface_operation c))
  references c = references (clientServerInterface_portInterface c)
  instantiates c = instantiates (clientServerInterface_portInterface c)

data PortInterface =
    ParameterInterfacePortInterface ParameterInterface
  | SenderReceiverInterfacePortInterface SenderReceiverInterface
  | NvDataInterfacePortInterface NvDataInterface
  | ModeSwitchInterfacePortInterface ModeSwitchInterface
  | TriggerInterfacePortInterface TriggerInterface
  | ClientServerInterfacePortInterface ClientServerInterface

deriving instance Eq PortInterface
deriving instance Show PortInterface

instance Referable PortInterface where
  shortName (ParameterInterfacePortInterface p) = shortName p
  -- TODO others

-----------------------------------------------------------------
-- SwConnector
-----------------------------------------------------------------

data SwConnector_  where SwConnector :: {
        swConnector_shortName       :: String,
        swConnector_mapping         :: Maybe (Ref PortInterfaceMapping),
        swConnector_variationPoint  :: Maybe VariationPoint
  } -> SwConnector_

deriving instance Show SwConnector_
deriving instance Eq SwConnector_

instance HasVariationPoint SwConnector_ where
  variationPoint = swConnector_variationPoint

instance HasSubElements SwConnector_ where
  aggregates sw = []
  references sw = map AnyRef (maybeToList (swConnector_mapping sw))
  instantiates sw = []

data SwConnector =
    AssemblySwConnector {
        swConnector                 :: SwConnector_,
        provider                    :: Maybe (IRef PortPrototype), -- Only PPort or PRPort
        requester                   :: Maybe (IRef PortPrototype) -- Only RPort or PRPort
    } |
    DelegationSwConnector {
        swConnector                 :: SwConnector_,
        innerPort                   :: IRef PortPrototype,
        outerPort                   :: Ref PortPrototype
    } |
    PassThroughSwConnector {
        swConnector                 :: SwConnector_,
        providedOuterPort           :: Ref PortPrototype, --  Only PPort or PRPort
        requiredOuterPort           :: Ref PortPrototype --  Only RPort or PRPort
    }
    deriving (Show, Eq)

instance Referable SwConnector where
  shortName s = swConnector_shortName (swConnector s)

instance ToElement SwConnector where
  toElement = SwConnectorElement

instance HasVariationPoint SwConnector where
  variationPoint = variationPoint . swConnector

instance HasSubElements SwConnector where
  aggregates c = aggregates (swConnector c)
  references (c@AssemblySwConnector{}) = []
  references (c@DelegationSwConnector{}) = [AnyRef (outerPort c)]
  references (c@PassThroughSwConnector{}) = [AnyRef (providedOuterPort c), AnyRef (requiredOuterPort c)]
  instantiates (c@AssemblySwConnector{}) = (map AnyIRef (maybeToList (provider c))) ++ (map AnyIRef (maybeToList (requester c)))
  instantiates (c@DelegationSwConnector{}) = [AnyIRef (innerPort c)]
  instantiates (c@PassThroughSwConnector{}) = []

-----------------------------------------------------------------
-- SwInternalBehavior
-----------------------------------------------------------------

data SwcInternalBehavior where
  SwcInternalBehavior :: {
        swcInternalBehavior_shortName   :: String,
        constantMemory                  :: Var ParameterDataPrototype, -- atpVariation, 0..*
        swcInternalBehavior_constantValueMapping :: [ConstantSpecificationMappingSet],
        dataTypeMapping                 :: [DataTypeMappingSet],
        exclusiveArea                   :: Var ExclusiveArea, -- atpVariation, 0..*
        swcInternalBehavior_exclusiveAreaNestingOrder :: Var ExclusiveAreaNestingOrder, -- atpVariation, atpSplitable, 0..*
        staticMemory                    :: Var VariableDataPrototype, -- atpVariation, 0..*
        arTypedPerInstanceMemory        :: Var VariableDataPrototype, -- atpVariation, 0..*
        event                           :: Var RTEEvent, -- atpVariation, atpSplitable, 0..*
        explicitInterRunnableVariable   :: Var VariableDataPrototype, -- atpVariation, 0..*
        handleTerminationAndRestart     :: HandleTerminationAndRestart,
        implicitInterRunnableVariable   :: Var VariableDataPrototype, -- atpVariation, 0..*
        includedDataTypeSet             :: [IncludedataTypeSet],
        includedModeDeclarationGroupSet :: [IncludedModeDeclarationGroupSet],
        swcInternalBehavior_instantiationDataDefProps :: Var InstantiationDataDefProps, -- atpVariation, 0..*
        swcInternalBehavior_perInstanceMemory :: Var PerInstanceMemory, -- atpVariation, 0..*
        perInstanceParameter            :: Var ParameterDataPrototype, -- atpVariation, atpSplitable, 0..*
        portAPIOption                   :: Var PortAPIOption, -- atpVariation, 0..*
        runnable                        :: Var RunnableEntity, -- atpVariation, atpSplitable, 1..*
        serviceDependency               :: Var ServiceDependency, -- atpVariation, 0..*
        sharedParameter                 :: Var ParameterDataPrototype, -- atpVariation, atpSplitable, 0..*
        supportsMultipleInstantiation   :: Bool,
        variationPointProxy             :: [VariationPointProxy],
        swcInternalBehavior_variationPoint :: Maybe VariationPoint
    } -> SwcInternalBehavior

deriving instance Show SwcInternalBehavior

instance Referable SwcInternalBehavior where
  shortName = swcInternalBehavior_shortName

instance ToElement SwcInternalBehavior where
  toElement = SwcInternalBehaviorElement

instance HasVariationPoint SwcInternalBehavior where
  variationPoint = swcInternalBehavior_variationPoint

instance HasSubElements SwcInternalBehavior where
  aggregates s = (map toElement (constantMemory s)) ++
                 (map toElement (swcInternalBehavior_constantValueMapping s)) ++
                 (map toElement (dataTypeMapping s)) ++
                 (map toElement (exclusiveArea s)) ++
                 (map toElement (swcInternalBehavior_exclusiveAreaNestingOrder s)) ++
                 (map toElement (staticMemory s)) ++
                 (map toElement (arTypedPerInstanceMemory s)) ++
                 (map toElement (event s)) ++
                 (map toElement (explicitInterRunnableVariable s)) ++
                 (map toElement (implicitInterRunnableVariable s)) ++
                 -- not referable: (map toElement (swcInternalBehavior_instantiationDataDefProps s)) ++
                 (map toElement (swcInternalBehavior_perInstanceMemory s)) ++
                 (map toElement (perInstanceParameter s)) ++
                 -- not referable: (map toElement (portAPIOption s)) ++
                 (map toElement (runnable s)) ++
                 (map toElement (serviceDependency s)) ++
                 (map toElement (sharedParameter s))
  references s = []
  instantiates s = []
                  
-- TODO? abstract DataPrototype with swDataDefProps :: SwDataDefProps

data ParameterDataPrototype where
  ParameterDataPrototype :: {
        parameterDataPrototype_shortName                 :: String,
        parameterDataPrototype_type                      :: Ref AutosarDataType, -- tref
        parameterDataPrototype_initValue :: Maybe ValueSpecification,
        parameterDataPrototype_variationPoint            :: Maybe VariationPoint
  } -> ParameterDataPrototype

deriving instance Eq ParameterDataPrototype
deriving instance Show ParameterDataPrototype

instance Referable ParameterDataPrototype where
  shortName = parameterDataPrototype_shortName

instance ToElement ParameterDataPrototype where
  toElement = ParameterDataPrototypeElement

instance HasVariationPoint ParameterDataPrototype where
  variationPoint = parameterDataPrototype_variationPoint

instance HasSubElements ParameterDataPrototype where
  aggregates p = []
  references p = [AnyRef (parameterDataPrototype_type p)]
  instantiates p = []

data VariableDataPrototype where
    VariableDataPrototype :: {
        variableDataPrototype_shortName                 :: String,
        variableDataPrototype_type                      :: Ref AutosarDataType,
        variableDataPrototype_initValue                 :: Maybe ValueSpecification,
        variableDataPrototype_variationPoint            :: Maybe VariationPoint
    } -> VariableDataPrototype

deriving instance Eq VariableDataPrototype
deriving instance Show VariableDataPrototype

instance Referable VariableDataPrototype where
  shortName = variableDataPrototype_shortName

instance ToElement VariableDataPrototype where
  toElement = VariableDataPrototypeElement

instance HasVariationPoint VariableDataPrototype where
  variationPoint = variableDataPrototype_variationPoint

instance HasSubElements VariableDataPrototype where
  aggregates _ = []
  references v = [AnyRef (variableDataPrototype_type v)]
  instantiates _ = []

data ArgumentDataPrototype where
  ArgumentDataPrototype :: {
        argumentDataPrototype_shortName :: String,
        argumentDataPrototype_type  :: Ref AutosarDataType,
        direction                   :: ArgumentDirectionEnum,
        serverArgumentImplPolicy    :: Maybe ServerArgumentImplPolicy,
        typeBlueprint               :: Var (VRef AutosarDataType), -- atpVariation, 0..1
        argumentDataPrototype_variationPoint :: Maybe VariationPoint
    } -> ArgumentDataPrototype

deriving instance Eq ArgumentDataPrototype
deriving instance Show ArgumentDataPrototype

instance Referable ArgumentDataPrototype where
  shortName = argumentDataPrototype_shortName

instance HasVariationPoint ArgumentDataPrototype where
  variationPoint = argumentDataPrototype_variationPoint

instance ToElement ArgumentDataPrototype where
  toElement = ArgumentDataPrototypeElement

instance HasSubElements ArgumentDataPrototype where
  aggregates a = []
  references a = [AnyRef (argumentDataPrototype_type a)] ++ (map (AnyRef . ref) (typeBlueprint a))
  instantiates a = []

data ArgumentDirectionEnum          = In | InOut | Out
    deriving (Show, Eq)

data DataPrototype =
    VariableDataPrototypeDataPrototype VariableDataPrototype
  | ParameterDataPrototypeDataPrototype ParameterDataPrototype
  | ArgumentDataPrototypeDataPrototype ArgumentDataPrototype
  -- ... some more
  deriving (Show, Eq)

data VariableAccess where VariableAccess :: {
        variableAccess_shortName    :: String,
        accessedVariable            :: AutosarVariableRef,
        scope                       :: Maybe VariableAccessScopeEnum,  -- for selected roles only
        variableAccess_variationPoint :: Maybe VariationPoint
    } -> VariableAccess

deriving instance Show VariableAccess

instance Referable VariableAccess where
  shortName = variableAccess_shortName

instance HasVariationPoint VariableAccess where
  variationPoint = variableAccess_variationPoint

instance ToElement VariableAccess where
  toElement = VariableAccessElement

instance HasSubElements VariableAccess where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data AutosarVariableRef =
        AutosarVariable                 DataPrototype |
        AutosarVariableInImplDatatype   ArVariableInImplementationDataInstanceRef |
        LocalVariable                   VariableDataPrototype
    deriving Show
        
data VariableAccessScopeEnum = 
        CommunicationInterEcu |
        CommunicationInterPartition |
        InterPartitionIntraEcu
    deriving Show
        
data Trigger = Trigger {
        trigger_shortName           :: String,
        swImplPolicy                :: Maybe SwImplPolicyEnum,
        triggerPeriod               :: Maybe MultidimensionalTime
    }
    deriving (Show, Eq)

instance Referable Trigger where
  shortName = trigger_shortName

data ClientServerOperation where
  ClientServerOperation :: {
        clientServerOperation_shortName                 :: String,
        clientServerOperation_argument                  :: Var ArgumentDataPrototype, -- atpVariation, 0..*
        clientServerOperation_possibleError             :: [ApplicationError],
        clientServerOperation_variationPoint            :: Maybe VariationPoint
    } -> ClientServerOperation

deriving instance Eq ClientServerOperation
deriving instance Show ClientServerOperation

instance Referable ClientServerOperation where
  shortName = clientServerOperation_shortName

instance ToElement ClientServerOperation where
  toElement = ClientServerOperationElement

instance HasVariationPoint ClientServerOperation where
  variationPoint = clientServerOperation_variationPoint

instance HasSubElements ClientServerOperation where
  aggregates c = map toElement (clientServerOperation_argument c)
  references _ = []
  instantiates _ = []

data AutosarDataType where
    AutosarDataType :: {
    autosarDataType_variationPoint               :: Maybe VariationPoint
    -- TODO swDataDefProps ??
    } -> AutosarDataType

deriving instance Eq AutosarDataType
deriving instance Show AutosarDataType

instance HasVariationPoint AutosarDataType where
  variationPoint = autosarDataType_variationPoint

data ApplicationError = ApplicationError {
        applicationError_shortName  :: String,
        errorCode                   :: Int
    }
    deriving (Show, Eq)

data HandleTerminationAndRestart =
    CanBeTerminated |
    CanBeTerminatedAndRestarted |
    NoSupport
    deriving Show


-----------------------------------------------------------------
-- Signals
-----------------------------------------------------------------

data SystemSignal where
  SystemSignal :: {
    systemSignal_shortName :: String,
    dynamicLength :: Bool,
    physicalProps :: Maybe SwDataDefProps,
    systemSignal_variationPoint :: Maybe VariationPoint
  } -> SystemSignal

deriving instance Show SystemSignal
deriving instance Eq SystemSignal

instance Referable SystemSignal where
  shortName = systemSignal_shortName

instance HasVariationPoint SystemSignal where
  variationPoint = systemSignal_variationPoint

instance HasSubElements SystemSignal where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data SystemSignalGroup where
  SystemSignalGroup :: {
    systemSignalGroup_shortName :: String,
    systemSignals :: [Ref SystemSignal],
    systemSignalGroup_variationPoint :: Maybe VariationPoint
  } -> SystemSignalGroup

deriving instance Show SystemSignalGroup
deriving instance Eq SystemSignalGroup

instance Referable SystemSignalGroup where
  shortName = systemSignalGroup_shortName

instance HasVariationPoint SystemSignalGroup where
  variationPoint = systemSignalGroup_variationPoint

instance HasSubElements SystemSignalGroup where
  aggregates _ = []
  references s = map AnyRef (systemSignals s)
  instantiates _ = []

-----------------------------------------------------------------
-- RTEEvent
-----------------------------------------------------------------

-- resolvePseudoVariation x = resolveA x

data RTEEvent_ where RTEEvent :: {
        rteEvent_shortName          :: String,
        disabledMode                :: [IRef ModeDeclaration],       -- atpSplitable
        startOnEvent                :: Maybe (Ref RunnableEntity),     -- invariant
        rteEvent_variationPoint     :: Maybe VariationPoint
    } -> RTEEvent_

deriving instance Show RTEEvent_

instance HasVariationPoint RTEEvent_ where
  variationPoint = rteEvent_variationPoint

instance HasSubElements RTEEvent_ where
  aggregates r = []
  references r = map AnyRef (maybeToList (startOnEvent r))
  instantiates r = map AnyIRef (disabledMode r)

data RTEEvent =
    AsynchronousServerCallReturnsEvent {
        rteEvent                    :: RTEEvent_,
        asynchronousServerCallReturnsEvent_eventSource :: Ref AsynchronousServerCallResultPoint
    } |
    DataSendCompletedEvent {
        rteEvent                    :: RTEEvent_,
        dataSendCompletedEvent_eventSource               :: Ref VariableAccess
    } |
    DataWriteCompletedEvent {
        rteEvent                    :: RTEEvent_,
        dataWriteCompletedEvent_eventSource               :: Ref VariableAccess
    } |
    DataReceivedEvent {
        rteEvent                    :: RTEEvent_,
        dataReceivedEvent_data      :: Maybe (IRef VariableDataPrototype)          -- invariant
    } |
    DataReceiveErrorEvent {
        rteEvent                    :: RTEEvent_,
        dataReceiveErrorEvent_data  :: Maybe (IRef VariableDataPrototype)          -- invariant
    } |
    OperationInvokedEvent {
        rteEvent                    :: RTEEvent_,
        operationInvokedEvent_operation :: Maybe (IRef ClientServerOperation)          -- invariant
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
        modeSwitchAckEvent_eventSource :: ModeSwitchPoint
    } |
    ExternalTriggerOccurredEvent {
        rteEvent                    :: RTEEvent_,
        externalTriggerOccurredEvent_trigger :: Maybe Trigger
    } |
    InternalTriggerOccurredEvent {
        rteEvent                    :: RTEEvent_,
        internalTriggerOccurredEvent_eventSource :: InternalTriggeringPoint
    } |
    InitEvent {
        rteEvent                    :: RTEEvent_
    }
    deriving Show

instance Referable RTEEvent where
  shortName r = rteEvent_shortName (rteEvent r)

instance ToElement RTEEvent where
  toElement = RTEEventElement

instance HasVariationPoint RTEEvent where
  variationPoint = variationPoint . rteEvent

instance HasSubElements RTEEvent where
  aggregates r = aggregates (rteEvent r)

  references r = (references (rteEvent r)) ++ (refs r)
     where refs v@AsynchronousServerCallReturnsEvent {} = [AnyRef (asynchronousServerCallReturnsEvent_eventSource v)]
           refs v@DataSendCompletedEvent {} = [AnyRef (dataSendCompletedEvent_eventSource v)]
           refs v@DataWriteCompletedEvent {} = [AnyRef (dataWriteCompletedEvent_eventSource v)]
           refs v@DataReceivedEvent {} = []
           refs v@DataReceiveErrorEvent {} = []
           refs v@OperationInvokedEvent {} = []
           refs v@TimingEvent {} = []
           refs v@BackGroundEvent {} = []
           refs v@SwcModeSwitchEvent {} = []
           refs v@ModeSwitchAckEvent {} = []
           refs v@ExternalTriggerOccurredEvent {} = []
           refs v@InternalTriggerOccurredEvent {} = []
           refs v@InitEvent {} = []

  instantiates r = (instantiates (rteEvent r)) ++ (insts r)
    where insts v@AsynchronousServerCallReturnsEvent {} = []
          insts v@DataSendCompletedEvent {} = []
          insts v@DataWriteCompletedEvent {} = []
          insts v@DataReceivedEvent {} = map AnyIRef (maybeToList (dataReceivedEvent_data v))
          insts v@DataReceiveErrorEvent {} = map AnyIRef (maybeToList (dataReceiveErrorEvent_data v))
          insts v@OperationInvokedEvent {} = map AnyIRef (maybeToList (operationInvokedEvent_operation v))
          insts v@TimingEvent {} = []
          insts v@BackGroundEvent {} = []
          insts v@SwcModeSwitchEvent {} = []
          insts v@ModeSwitchAckEvent {} = []
          insts v@ExternalTriggerOccurredEvent {} = []
          insts v@InternalTriggerOccurredEvent {} = []
          insts v@InitEvent {} = []

data ModeActivationKind             = OnEntry | OnExit | OnTransition
    deriving Show

-----------------------------------------------------------------
-- Runnable
-----------------------------------------------------------------

-- Note: pseudo variation means: The element type is used in an
-- atpVariation context elsewhere, but is reused here in
-- an invariant position.

data ExecutableEntity where ExecutableEntity :: {
        activationReason                    :: [ExecutableEntityActivationReason],
        canEnterExclusiveArea               :: Var ExclusiveArea, -- pseudo variation, 0..*
        executableEntity_exclusiveAreaNestingOrder         :: Var ExclusiveAreaNestingOrder, -- pseudo variation, 0..*
        minimumStartInterval                :: TimeValue,
        reentrancyLevel                     :: Maybe ReentrancyLevelEnum,   -- only for BSW
        runsInsideExclusiveArea             :: Var ExclusiveArea, -- pseudo variation, 0..*
        swAddrMethod                        :: Maybe SwAddrMethod
    } -> ExecutableEntity

deriving instance Show ExecutableEntity

data RunnableEntity where
  RunnableEntity :: {
        runnableEntity_shortName           :: String,
        executableEntity                    :: ExecutableEntity,
        runnableEntity_argument             :: [RunnableEntityArgument],    -- client/server only
        asynchronousServerCallResultPoint   :: Var AsynchronousServerCallResultPoint, -- atpVariation, 0..*
        canBeInvokedConcurrently            :: Bool, -- ^ concurrently with itself
        dataReadAccess                      :: Var VariableAccess,    -- cat 1 only, atpVariation, 0..*
        dataReceivePointByArgument          :: Var VariableAccess,    -- send/rcv only, atpVariation, 0..*
        dataReceivePointByValue             :: Var VariableAccess,    -- send/rcv only, atpVariation, 0..*
        dataSendPoint                       :: Var VariableAccess,    -- send/rcv only, atpVariation, 0..*
        dataWriteAccess                     :: Var VariableAccess,    -- cat 1 only, atpVariation, 0..*
        externalTriggeringPoint             :: Var ExternalTriggeringPoint, -- atpVariation, 0..*
        internalTriggeringPoint             :: Var InternalTriggeringPoint, -- atpVariation, 0..*
        modeAccessPoint                     :: Var ModeAccessPoint, -- atpVariation, 0..*
        modeSwitchPoint                     :: Var ModeSwitchPoint, -- atpVariation, 0..*
        parameterAccess                     :: Var ParameterAccess, -- atpVariation, 0..*
        readLocalVariable                   :: Var VariableAccess, -- atpVariation, 0..*
        serverCallPoint                     :: Var ServerCallPoint, -- atpVariation, 0..*
        symbol                              :: CIdentifier,
        waitPoint                           :: [WaitPoint],         -- not null <=> cat 2
        writtenLocalVariable                :: Var VariableAccess, -- atpVariation, 0..*
        runnableEntity_variationPoint       :: Maybe VariationPoint
    } -> RunnableEntity

deriving instance Show RunnableEntity

instance Referable RunnableEntity where
  shortName = runnableEntity_shortName

instance ToElement RunnableEntity where
  toElement = RunnableEntityElement

instance HasVariationPoint RunnableEntity where
  variationPoint = runnableEntity_variationPoint

instance HasSubElements RunnableEntity where
  aggregates r = -- not an element: [toElement (executableEntity r)] ++
                 (map toElement (asynchronousServerCallResultPoint r)) ++
                 (map toElement (dataReadAccess r)) ++
                 (map toElement (dataReceivePointByArgument r)) ++
                 (map toElement (dataReceivePointByValue r)) ++
                 (map toElement (dataSendPoint r)) ++
                 (map toElement (dataWriteAccess r)) ++
                 -- not en element: (map toElement (externalTriggeringPoint r)) ++
                 (map toElement (internalTriggeringPoint r)) ++
                 -- not an element: (map toElement (modeAccessPoint r)) ++
                 (map toElement (modeSwitchPoint r)) ++
                 (map toElement (parameterAccess r)) ++
                 (map toElement (readLocalVariable r)) ++
                 (map toElement (serverCallPoint r)) ++
                 (map toElement (writtenLocalVariable r))
  references _ = []
  instantiates _ = []

-----------------------------------------------------------------
-- SwImplementation
-----------------------------------------------------------------

data SwcImplementation where
   SwcImplementation :: {
        implementation                      :: Implementation, -- a "base"
        behavior                            :: Ref SwcInternalBehavior,
        perInstanceMemorySize               :: Var PerInstanceMemorySize, -- atpVariation, 0..*
        requiredRTEVendor                   :: Maybe String
    } -> SwcImplementation

deriving instance Show SwcImplementation

instance Referable SwcImplementation where
  shortName = implementation_shortName . implementation

instance HasVariationPoint SwcImplementation where
  variationPoint = variationPoint . implementation

instance HasSubElements SwcImplementation where
  aggregates s = aggregates (implementation s)
                 -- not referable: (map toElement (perInstanceMemorySize s))
  references s = (references (implementation s)) ++ [AnyRef (behavior s)]
  instantiates s = (instantiates (implementation s))

-- FIXME: this should really be the common part of a sum type
data Implementation where
   Implementation :: {
        implementation_shortName            :: String,
        implementation_variationPoint       :: Maybe VariationPoint,
        buildActionManifest                 :: Var String, -- TODO atpVariation, 0..1 (type should be BuildActionManifest)
        codeDescriptor                      :: [String], -- 1..*
        compiler                            :: [String],
        generatedArtifact                   :: [String], -- TODO atpVariation, 0..* (type should be DependencyOnArtifact)
        hwElement                           :: [Ref HwElement],
        linker                              :: [String],
        -- mcSupport missing?
        programmingLanguage                 :: ProgrammingLanguageEnum,
        requiredArtifact                    :: [String], -- TODO atpVariation, 0..* (type should be DependencyOnArtifact)
        requiredGeneratorTool               :: [String], -- TODO atpVariation, 0..* (type should be DependencyOnArtifact)
        resourceConsumption                 :: ResourceConsumption,
        swVersion                           :: String,
        swcBswMapping                       :: Maybe (Ref SwcBswMapping),
        usedCodeGenerator                   :: Maybe String,
        vendorId                            :: Int
    } -> Implementation

deriving instance Show Implementation

instance HasSubElements Implementation where
  aggregates _ = []
  references i = (map AnyRef (hwElement i)) ++ (map AnyRef (maybeToList (swcBswMapping i)))
  instantiates _ = []

instance HasVariationPoint Implementation where
  variationPoint = implementation_variationPoint

data PerInstanceMemory where PerInstanceMemory :: {
        perInstanceMemory_shortName         :: String,
        initValue                           :: Maybe String,
        swDataDefProps                      :: SwDataDefProps,
        perInstanceMemory_type              :: String,
        typeDefinition                      :: String,
        perInstanceMemory_variationPoint    :: Maybe VariationPoint
    } -> PerInstanceMemory

deriving instance Show PerInstanceMemory

instance Referable PerInstanceMemory where
  shortName = perInstanceMemory_shortName

instance ToElement PerInstanceMemory where
  toElement = PerInstanceMemoryElement

instance HasVariationPoint PerInstanceMemory where
  variationPoint = perInstanceMemory_variationPoint

instance HasSubElements PerInstanceMemory where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data PerInstanceMemorySize where PerInstanceMemorySize :: {
        alignment                           :: Integer,
        perInstanceMemorySize_perInstanceMemory :: PerInstanceMemory,
        size                                :: Integer, -- TODO atpVariation (on attr!)
        perInstanceMemorySize_variationPoint :: Maybe VariationPoint
    } -> PerInstanceMemorySize

deriving instance Show PerInstanceMemorySize

data PortInterfaceMapping_ where
  PortInterfaceMapping :: {
    portInterfaceMapping_shortName :: String,
    portInterfaceMapping_variationPoint :: Maybe VariationPoint
    } -> PortInterfaceMapping_

deriving instance Show PortInterfaceMapping_

data PortInterfaceMapping where
  VariableAndParameterInterfaceMapping :: {
    portInterfaceMapping :: PortInterfaceMapping_,
    dataPrototypeMapping :: [DataPrototypeMapping] -- 1..*
  } -> PortInterfaceMapping
  ClientServerInterfaceMapping :: {
    portInterfaceMapping :: PortInterfaceMapping_,
    operationMapping :: [ClientServerOperationMapping] -- 1..*
  } -> PortInterfaceMapping
  ModeInterfaceMapping :: {
    portInterfaceMapping :: PortInterfaceMapping_,
    modeMapping :: ModeDeclarationGroupPrototypeMapping
  } -> PortInterfaceMapping
  TriggerInterfaceMapping :: {
    portInterfaceMapping :: PortInterfaceMapping_,
    triggerMapping :: [TriggerMapping] -- 1..*
  } -> PortInterfaceMapping

deriving instance Show PortInterfaceMapping

instance Referable PortInterfaceMapping where
  shortName p = portInterfaceMapping_shortName (portInterfaceMapping p)

instance HasVariationPoint PortInterfaceMapping where
  variationPoint p = portInterfaceMapping_variationPoint (portInterfaceMapping p)

instance ToElement PortInterfaceMapping where
  toElement = PortInterfaceMappingElement

instance HasSubElements PortInterfaceMapping where
  aggregates _ = [] -- FIXME HARD
  references _ = []
  instantiates _ = []

data DataPrototypeMapping where
 DataPrototypeMapping :: {
   firstDataPrototype :: Ref AutosarDataPrototype,
   secondDataPrototype :: Ref AutosarDataPrototype,
   subElementMapping :: [SubElementMapping],
   textTableMapping :: [TextTableMapping] -- 0..2
 } -> DataPrototypeMapping

deriving instance Show DataPrototypeMapping

data SubElementMapping where
  SubElementMapping :: {
    firstElement :: Var SubElementRef, -- 0..1, atpVariation
    secondElement :: Var SubElementRef, -- 0..1, atpVariation
    subTextTableMapping :: [TextTableMapping] -- 0..2
  } -> SubElementMapping

deriving instance Show SubElementMapping

data SubElementRef where
  ImplementationDataTypeSubElementRef :: {
    implementationDataTypeElement :: ArVariableInImplementationDataInstanceRef,
    subElementRef_variationPoint :: Maybe VariationPoint
  } -> SubElementRef
  ApplicationCompositeDataTypeSubElementRef :: {
    applicationCompositeElement :: IRef ApplicationCompositeElementDataPrototype,
    subElementRef_variationPoint :: Maybe VariationPoint
  } -> SubElementRef

deriving instance Show SubElementRef

data TextTableMapping where
  TextTableMapping :: {
    identicalMapping :: Bool,
    mappingDirection :: Maybe MappingDirectionEnum, -- pureMM.minOccurs="1"
    valuePair :: [TextTableValuePair]
  } -> TextTableMapping

deriving instance Show TextTableMapping

data TextTableValuePair where
  TextTableValuePair :: {
    firstValue :: VariationPoint,
    secondValue :: VariationPoint
  } -> TextTableValuePair

deriving instance Show TextTableValuePair

data MappingDirectionEnum =
    Bidirectional
  | FirstToSecond
  | SecondToFirst
  deriving (Show, Eq, Ord)

data ClientServerOperationMapping where
  ClientServerOperatiioMapping :: {
    argumentMapping :: [DataPrototypeMapping],
    firstOperation :: Ref ClientServerOperation,
    secondOperation :: Ref ClientServerOperation
  } -> ClientServerOperationMapping

deriving instance Show ClientServerOperationMapping

data TriggerMapping where
  TriggerMapping :: {
    firstTrigger :: Ref Trigger,
    secondTrigger :: Ref Trigger
  } -> TriggerMapping

deriving instance Show TriggerMapping

data ModeDeclarationGroupPrototypeMapping where
  ModeDeclarationGroupPrototypeMapping :: {
    firstModeGroup :: Ref ModeDeclarationGroupPrototype,
    secondModeGroup :: Ref ModeDeclarationGroupPrototype
  } -> ModeDeclarationGroupPrototypeMapping

deriving instance Show ModeDeclarationGroupPrototypeMapping

data PortInterfaceMappingSet where
  PortInterfaceMappingSet :: {
    portInterfaceMappingSet_shortName :: String,
    portInterfaceMappings :: Var PortInterfaceMapping, -- 1..*,
                            -- atpVariation BlueprintDerivationTime
    portInterfaceMappingSet_variationPont :: Maybe VariationPoint
    } -> PortInterfaceMappingSet

deriving instance Show PortInterfaceMappingSet

instance Referable PortInterfaceMappingSet where
  shortName = portInterfaceMappingSet_shortName
instance HasVariationPoint PortInterfaceMappingSet where
  variationPoint = portInterfaceMappingSet_variationPont
instance HasSubElements PortInterfaceMappingSet where
  aggregates s = map toElement (portInterfaceMappings s)
  references _ = []
  instantiates _ = []

instance ToElement PortInterfaceMappingSet where
  toElement = PortInterfaceMappingSetElement

-----------------------------------------------------------------
-- Misc.
-----------------------------------------------------------------

type TimeValue = Double
type CIdentifier = String
type ExecutableEntityActivationReason = Int
type RunnableEntityArgument = CIdentifier


data ConsistencyNeeds where
   ConsistencyNeeds :: {
     consistencyNeeds_shortName :: String,
     concistencyNeeds_variationPoint :: Maybe VariationPoint
   } -> ConsistencyNeeds

deriving instance Show ConsistencyNeeds

instance Referable ConsistencyNeeds where
  shortName = consistencyNeeds_shortName

instance ToElement ConsistencyNeeds where
  toElement = ConsistencyNeedsElement

instance HasVariationPoint ConsistencyNeeds where
  variationPoint = concistencyNeeds_variationPoint

instance HasSubElements ConsistencyNeeds where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data PortGroup where
  PortGroup :: {
    portGroup_shortName :: String,
    portGroup_variationPoint :: Maybe VariationPoint
  } -> PortGroup

deriving instance Show PortGroup

instance Referable PortGroup where
  shortName = portGroup_shortName

instance ToElement PortGroup where
  toElement = PortGroupElement

instance HasVariationPoint PortGroup where
  variationPoint = portGroup_variationPoint

instance HasSubElements PortGroup where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data SwComponentDocumentation = SwComponentDocumentation deriving (Show, Eq)
instance HasVariationPoint SwComponentDocumentation where
  variationPoint = undefined -- FIXME


data UnitGroup = UnitGroup
 deriving (Show, Eq)

instance Referable UnitGroup where
  shortName = undefined -- FIXME
instance HasVariationPoint UnitGroup where
  variationPoint x = undefined -- FIXME
instance HasSubElements UnitGroup where
  aggregates _ = []
  references _ = []
  instantiates _ = []

type SymbolProps = ()

data ConstantSpecificationMappingSet where
  ConstantSpecificationMappingSet :: {
    constantSpecificationMappingSet_shortName :: String
  } -> ConstantSpecificationMappingSet

instance Referable ConstantSpecificationMappingSet where
  shortName = constantSpecificationMappingSet_shortName

instance ToElement ConstantSpecificationMappingSet where
  toElement = ConstantSpecificationMappingSetElement

deriving instance Show ConstantSpecificationMappingSet

instance HasVariationPoint ConstantSpecificationMappingSet where
  variationPoint x = undefined -- FIXME

instance HasSubElements ConstantSpecificationMappingSet where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data DataTypeMappingSet where
  DataTypeMappingSet :: {
    dataTypeMappingSet_shortName :: String,
    dataTypeMappingSet_variationPoint :: Maybe VariationPoint
  } -> DataTypeMappingSet

deriving instance Show DataTypeMappingSet

instance Referable DataTypeMappingSet where
  shortName = dataTypeMappingSet_shortName

instance ToElement DataTypeMappingSet where
  toElement = DataTypeMappingSetElement

instance HasVariationPoint DataTypeMappingSet where
  variationPoint = dataTypeMappingSet_variationPoint

instance HasSubElements DataTypeMappingSet where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data InstantiationDataDefProps = InstantiationDataDefProps deriving (Show, Eq)
instance HasVariationPoint InstantiationDataDefProps where
  variationPoint = undefined -- FIXME

data InstantiationRTEEventProps = InstantiationRTEEventProps deriving (Show, Eq)
instance HasVariationPoint InstantiationRTEEventProps where
  variationPoint = undefined -- FIXME

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

data ModeDeclarationGroupPrototype = ModeDeclarationGroupPrototype {
  modeDeclarationGroupPrototype_shortName :: String,
  swCalibrationAccess :: Maybe SwCalibrationAccessEnum,
  modeDeclarationGroupPrototype_type :: Ref ModeDeclarationGroup  -- tref
  } deriving Show

instance Referable ModeDeclarationGroupPrototype where
  shortName = modeDeclarationGroupPrototype_shortName

instance Eq ModeDeclarationGroupPrototype where
  (==) = undefined

type ModeDeclarationGroup = ()

data SwCalibrationAccessEnum =
  SCANotAccessible | SCAReadOnly | SCAReadWrite
  deriving (Show, Eq, Ord)

type SwImplPolicyEnum = ()
type MultidimensionalTime = ()
type ValueSpecification = ()
type ServerArgumentImplPolicy = ()
type VariationPointProxy = ()

--- FIXME: this is the common part of a sum type
data ServiceDependency where
  ServiceDependency :: {
    serviceDependency_shortName :: String,
    serviceDependency_variationPoint :: Maybe VariationPoint
  } -> ServiceDependency

deriving instance Show ServiceDependency

instance Referable ServiceDependency where
  shortName = serviceDependency_shortName

instance ToElement ServiceDependency where
  toElement = ServiceDependencyElement

instance HasVariationPoint ServiceDependency where
  variationPoint = serviceDependency_variationPoint

instance HasSubElements ServiceDependency where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data PortAPIOption = PortAPIOption deriving (Show, Eq)
instance HasVariationPoint PortAPIOption where
  variationPoint = undefined -- FIXME

type IncludedModeDeclarationGroupSet = ()
type IncludedataTypeSet = ()
type ModeDeclaration = ()

data AsynchronousServerCallResultPoint where
  AsynchronousServerCallResultPoint :: {
    asynchronousServerCallResultPoint_shortName :: String,
    asynchronousServerCallResultPoint_variationPoint :: Maybe VariationPoint
  } -> AsynchronousServerCallResultPoint

deriving instance Show AsynchronousServerCallResultPoint

instance Referable AsynchronousServerCallResultPoint where
  shortName = asynchronousServerCallResultPoint_shortName

instance HasVariationPoint AsynchronousServerCallResultPoint where
  variationPoint = asynchronousServerCallResultPoint_variationPoint

instance ToElement AsynchronousServerCallResultPoint where
  toElement = AsynchronousServerCallResultPointElement

instance HasSubElements AsynchronousServerCallResultPoint where
  aggregates _ = []
  references _ = []
  instantiates _ = []

type WaitPoint = ()

data ServerCallPoint where
  ServerCallPoint :: {
    serverCallPoint_shortName :: String,
    serverCallPoint_variationPoint :: Maybe VariationPoint
  } -> ServerCallPoint

deriving instance (Show ServerCallPoint)

instance Referable ServerCallPoint where
  shortName = serverCallPoint_shortName

instance HasVariationPoint ServerCallPoint where
  variationPoint = serverCallPoint_variationPoint

instance ToElement ServerCallPoint where
  toElement = ServerCallPointElement

instance HasSubElements ServerCallPoint where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data ParameterAccess where
  ParameterAccess :: {
    parameterAccess_shortName :: String,
    parameterAccess_variationPoint :: Maybe VariationPoint
  }  -> ParameterAccess

deriving instance Show ParameterAccess

instance Referable ParameterAccess where
  shortName = parameterAccess_shortName

instance HasVariationPoint ParameterAccess where
  variationPoint = parameterAccess_variationPoint

instance ToElement ParameterAccess where
  toElement = ParameterAccessElement

instance HasSubElements ParameterAccess where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data ModeSwitchPoint where
  ModeSwitchPoint :: {
    modeSwitchPoint_shortName :: String,
    modeSwitchPoint_variationPoint :: Maybe VariationPoint
  } -> ModeSwitchPoint

deriving instance Show ModeSwitchPoint

instance Referable ModeSwitchPoint where
  shortName = modeSwitchPoint_shortName

instance HasVariationPoint ModeSwitchPoint where
  variationPoint = modeSwitchPoint_variationPoint

instance ToElement ModeSwitchPoint where
  toElement = ModeSwitchPointElement

instance HasSubElements ModeSwitchPoint where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data ModeAccessPoint = ModeAccessPoint deriving (Show, Eq)
instance HasVariationPoint ModeAccessPoint where
  variationPoint = undefined -- FIXME

data InternalTriggeringPoint where
  InternalTriggeringPoint :: {
    internalTriggeringPoint_shortName :: String,
    internalTriggeringPoint_variationPoint :: Maybe VariationPoint
  } -> InternalTriggeringPoint

deriving instance  Show InternalTriggeringPoint

instance Referable InternalTriggeringPoint where
  shortName = internalTriggeringPoint_shortName

instance HasVariationPoint InternalTriggeringPoint where
  variationPoint = internalTriggeringPoint_variationPoint

instance ToElement InternalTriggeringPoint where
  toElement = InternalTriggeringPointElement

instance HasSubElements InternalTriggeringPoint where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data ExternalTriggeringPoint = ExternalTriggeringPoint deriving (Show, Eq)
instance HasVariationPoint ExternalTriggeringPoint where
  variationPoint = undefined -- FIXME

data SwcBswMapping = SwcBswMapping
  deriving (Show, Eq)

instance Referable SwcBswMapping where
  shortName = undefined -- FIXME
instance HasVariationPoint SwcBswMapping where
  variationPoint x = undefined -- FIXME
instance HasSubElements SwcBswMapping where
  aggregates _ = []
  references _ = []
  instantiates _ = []

type ResourceConsumption = ()
type ProgrammingLanguageEnum = ()

data HwElement = HwElement {
  hwElement_shortName :: String
  } deriving (Show, Eq)

instance Referable HwElement where
  shortName = hwElement_shortName
instance HasVariationPoint HwElement where
  variationPoint x = undefined -- FIXME
instance HasSubElements HwElement where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data ExclusiveAreaNestingOrder where
  ExclusiveAreaNestingOrder :: {
    exclusiveAreaNestingOrder_shortName :: String,
    exclusiveAreaNestingOrder_variationPoint :: Maybe VariationPoint
  } -> ExclusiveAreaNestingOrder

deriving instance Show ExclusiveAreaNestingOrder

instance Referable ExclusiveAreaNestingOrder where
  shortName = exclusiveAreaNestingOrder_shortName

instance ToElement ExclusiveAreaNestingOrder where
  toElement = ExclusiveAreaNestingOrderElement

instance HasVariationPoint ExclusiveAreaNestingOrder where
  variationPoint = exclusiveAreaNestingOrder_variationPoint

instance HasSubElements ExclusiveAreaNestingOrder where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data ExclusiveArea where
  ExclusiveArea :: {
    exclusiveArea_shortName :: String,
    exclusiveArea_variationPoint :: Maybe VariationPoint
  } -> ExclusiveArea

deriving instance (Show ExclusiveArea)

instance Referable ExclusiveArea where
  shortName = exclusiveArea_shortName

instance ToElement ExclusiveArea where
  toElement = ExclusiveAreaElement

instance HasVariationPoint ExclusiveArea where
  variationPoint = exclusiveArea_variationPoint

instance HasSubElements ExclusiveArea where
  aggregates _ = []
  references _ = []
  instantiates _ = []

type SwDataDefProps = ()

data FlatMap where
  FlatMap :: {
    flatMap_shortName :: String,
    flatMap_flatInstanceDescriptor :: Var FlatInstanceDescriptor, -- 1..*,  atpSplitable; atpVariation
    flatMap_variationPoint :: Maybe VariationPoint
    } -> FlatMap

deriving instance Show FlatMap

instance Referable FlatMap where
  shortName = flatMap_shortName

instance HasSubElements FlatMap where
  aggregates _ = []
  references _ = []
  instantiates _ = []

instance HasVariationPoint FlatMap where
  variationPoint = flatMap_variationPoint

data FlatInstanceDescriptor where
  FlatInstanceDescriptor :: {
    flatInstanceDescriptor_shortName :: String
    -- TODO more if interesting?
    } -> FlatInstanceDescriptor

deriving instance Show FlatInstanceDescriptor

instance Referable FlatInstanceDescriptor where
  shortName = flatInstanceDescriptor_shortName

data CalibrationParameterValueSet = CalibrationParameterValueSet
  deriving (Show, Eq)

instance Referable CalibrationParameterValueSet where
  shortName = undefined -- FIXME
instance HasVariationPoint CalibrationParameterValueSet where
  variationPoint = undefined -- FIXME
instance HasSubElements CalibrationParameterValueSet where
  aggregates _ = []
  references _ = []
  instantiates _ = []

type EcuPartition = ()
data EcuInstance = EcuInstance {
  ecuInstance_shortName :: String
  } deriving (Show, Eq) -- FIXME

instance Referable EcuInstance where
  shortName = ecuInstance_shortName

instance HasVariationPoint EcuInstance where
  variationPoint = undefined -- FIXME
instance HasSubElements EcuInstance where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data SignalPathConstraint = SignalPathConstraint deriving (Show, Eq)
instance HasVariationPoint SignalPathConstraint where
  variationPoint = undefined -- FIXME

data EcuResourceEstimation = EcuResourceEstimation deriving (Show, Eq)
instance HasVariationPoint EcuResourceEstimation where
  variationPoint = undefined -- FIXME

data PncMapping = PncMapping deriving (Show, Eq)
instance HasVariationPoint PncMapping where
  variationPoint = undefined -- FIXME

data EcuMapping where
  EcuMapping :: {
    ecuMapping_shortName :: String
  } -> EcuMapping

deriving instance Show EcuMapping
deriving instance Eq EcuMapping

instance ToElement EcuMapping where
  toElement = EcuMappingElement

instance Referable EcuMapping where
  shortName = ecuMapping_shortName

instance HasSubElements EcuMapping where
  aggregates _ = []
  references _ = []
  instantiates _ = []

instance HasVariationPoint EcuMapping where
  variationPoint = undefined -- FIXME

data DataMapping_ =
  DataMapping {
    communicationDirectionType :: Maybe CommunicationDirectionType,
    dataMapping_variationPoint :: Maybe VariationPoint
    -- FIXME: more are in the spec
  }
  deriving (Show, Eq)

data CommunicationDirectionType = CommunicationDirectionIn | CommunicationDirectionOut
  deriving (Show, Eq)

data DataMapping where
  SenderReceiverToSignalMapping :: {
    dataMapping_ :: DataMapping_,
    mapping_dataElement :: IRef VariableDataPrototype,
    systemSignal :: Ref SystemSignal
    
  } -> DataMapping
  SenderReceiverToSignalGroupMapping :: {
    dataMapping_ :: DataMapping_,
    mapping_dataElement :: IRef VariableDataPrototype,
    signalGroup :: Ref SystemSignalGroup,
    typeMapping :: SenderRecCompositeTypeMapping
  } -> DataMapping
  ClientServerToSignalMapping :: {
    dataMapping_ :: DataMapping_,
    callSignal :: Ref SystemSignal,
    clientServerOperation :: IRef ClientServerOperation,
    lengthClientId :: Maybe Int,
    lengthSequenceCounter :: Maybe Int,
    returnSignal :: Maybe (Ref SystemSignal),
    serializer :: Ref SerializationTechnology
  } -> DataMapping

deriving instance Show DataMapping 
deriving instance Eq DataMapping 

instance HasVariationPoint DataMapping where
  variationPoint = dataMapping_variationPoint . dataMapping_

type SenderRecCompositeTypeMapping = ()
type SerializationTechnology = ()

data FibexElement = FibexElement deriving (Show, Eq)
instance HasVariationPoint FibexElement where
  variationPoint = undefined -- FIXME
instance HasSubElements FibexElement where
  aggregates _ = []
  references _ = []
  instantiates _ = []

data SwAddrMethod = SwAddrMethod
  deriving (Show, Eq)

instance Referable SwAddrMethod where
  shortName = undefined -- FIXME
instance HasVariationPoint SwAddrMethod where
  variationPoint x = undefined -- FIXME
instance HasSubElements SwAddrMethod where
  aggregates _ = []
  references _ = []
  instantiates _ = []

type ReentrancyLevelEnum = ()
type ArVariableInImplementationDataInstanceRef = ()

type AutosarDataPrototype = ()

type ApplicationCompositeElementDataPrototype = ()

data ReferrableSubtypesEnum =
    RSEArElement
  | RSEArPackage
  | RSEAbstractCanCluster
  | RSEAbstractCanCommunicationConnector
  | RSEAbstractCanCommunicationController
  | RSEAbstractCanPhysicalChannel
  | RSEAbstractEvent
  | RSEAbstractProvidedPortPrototype
  | RSEAbstractRequiredPortPrototype
  | RSEAbstractServiceInstance
  | RSEAclObjectSet
  | RSEAclOperation
  | RSEAclPermission
  | RSEAclRole
  | RSEAgeConstraint
  | RSEAliasNameSet
  | RSEAnalyzedExecutionTime
  | RSEApplicationArrayDataType
  | RSEApplicationArrayElement
  | RSEApplicationCompositeDataType
  | RSEApplicationCompositeElementDataPrototype
  | RSEApplicationDataType
  | RSEApplicationEndpoint
  | RSEApplicationError
  | RSEApplicationPrimitiveDataType
  | RSEApplicationRecordDataType
  | RSEApplicationRecordElement
  | RSEApplicationSwComponentType
  | RSEArbitraryEventTriggering
  | RSEArgumentDataPrototype
  | RSEAssemblySwConnector
  | RSEAsynchronousServerCallPoint
  | RSEAsynchronousServerCallResultPoint
  | RSEAsynchronousServerCallReturnsEvent
  | RSEAtomicSwComponentType
  | RSEAtpBlueprint
  | RSEAtpBlueprintable
  | RSEAtpClassifier
  | RSEAtpDefinition
  | RSEAtpFeature
  | RSEAtpPrototype
  | RSEAtpStructureElement
  | RSEAtpType
  | RSEAutosarDataPrototype
  | RSEAutosarDataType
  | RSEAutosarOperationArgumentInstance
  | RSEAutosarVariableInstance
  | RSEBackgroundEvent
  | RSEBaseType
  | RSEBlueprintMappingSet
  | RSEBswAsynchronousServerCallPoint
  | RSEBswAsynchronousServerCallResultPoint
  | RSEBswAsynchronousServerCallReturnsEvent
  | RSEBswBackgroundEvent
  | RSEBswCalledEntity
  | RSEBswDataReceivedEvent
  | RSEBswDebugInfo
  | RSEBswDirectCallPoint
  | RSEBswDistinguishedPartition
  | RSEBswEvent
  | RSEBswExternalTriggerOccurredEvent
  | RSEBswImplementation
  | RSEBswInternalBehavior
  | RSEBswInternalTriggerOccurredEvent
  | RSEBswInternalTriggeringPoint
  | RSEBswInterruptEntity
  | RSEBswMgrNeeds
  | RSEBswModeManagerErrorEvent
  | RSEBswModeSwitchEvent
  | RSEBswModeSwitchedAckEvent
  | RSEBswModuleCallPoint
  | RSEBswModuleClientServerEntry
  | RSEBswModuleDependency
  | RSEBswModuleDescription
  | RSEBswModuleEntity
  | RSEBswModuleEntry
  | RSEBswModuleTiming
  | RSEBswOperationInvokedEvent
  | RSEBswSchedulableEntity
  | RSEBswScheduleEvent
  | RSEBswSchedulerNamePrefix
  | RSEBswSynchronousServerCallPoint
  | RSEBswTimingEvent
  | RSEBswVariableAccess
  | RSEBuildAction
  | RSEBuildActionEntity
  | RSEBuildActionEnvironment
  | RSEBuildActionManifest
  | RSEBurstPatternEventTriggering
  | RSECalibrationParameterValueSet
  | RSECanCluster
  | RSECanCommunicationConnector
  | RSECanCommunicationController
  | RSECanFrame
  | RSECanFrameTriggering
  | RSECanNmCluster
  | RSECanNmNode
  | RSECanPhysicalChannel
  | RSECanTpAddress
  | RSECanTpChannel
  | RSECanTpConfig
  | RSECanTpNode
  | RSECaption
  | RSEChapter
  | RSEClientServerInterface
  | RSEClientServerInterfaceMapping
  | RSEClientServerOperation
  | RSECode
  | RSECollectableElement
  | RSECollection
  | RSEComMgrUserNeeds
  | RSECommConnectorPort
  | RSECommunicationCluster
  | RSECommunicationConnector
  | RSECommunicationController
  | RSECompiler
  | RSEComplexDeviceDriverSwComponentType
  | RSECompositionSwComponentType
  | RSECompuMethod
  | RSEConcretePatternEventTriggering
  | RSEConsistencyNeeds
  | RSEConsistencyNeedsBlueprintSet
  | RSEConstantSpecification
  | RSEConstantSpecificationMappingSet
  | RSEConsumedEventGroup
  | RSEConsumedServiceInstance
  | RSECouplingElement
  | RSECouplingPort
  | RSECryptoServiceNeeds
  | RSEDataConstr
  | RSEDataInterface
  | RSEDataPrototype
  | RSEDataPrototypeGroup
  | RSEDataReceiveErrorEvent
  | RSEDataReceivedEvent
  | RSEDataSendCompletedEvent
  | RSEDataTypeMappingSet
  | RSEDataWriteCompletedEvent
  | RSEDcmIPdu
  | RSEDefItem
  | RSEDelegationSwConnector
  | RSEDependencyOnArtifact
  | RSEDiagEventDebounceAlgorithm
  | RSEDiagEventDebounceCounterBased
  | RSEDiagEventDebounceMonitorInternal
  | RSEDiagEventDebounceTimeBased
  | RSEDiagnosticCapabilityElement
  | RSEDiagnosticCommunicationManagerNeeds
  | RSEDiagnosticEnableConditionNeeds
  | RSEDiagnosticEventInfoNeeds
  | RSEDiagnosticEventManagerNeeds
  | RSEDiagnosticEventNeeds
  | RSEDiagnosticIoControlNeeds
  | RSEDiagnosticOperationCycleNeeds
  | RSEDiagnosticRoutineNeeds
  | RSEDiagnosticStorageConditionNeeds
  | RSEDiagnosticValueNeeds
  | RSEDiagnosticsCommunicationSecurityNeeds
  | RSEDltUserNeeds
  | RSEDoIpActivationLineNeeds
  | RSEDoIpGidNeeds
  | RSEDoIpGidSynchronizationNeeds
  | RSEDoIpPowerModeStatusNeeds
  | RSEDoIpRoutingActivationAuthenticationNeeds
  | RSEDoIpRoutingActivationConfirmationNeeds
  | RSEDoIpServiceNeeds
  | RSEDocumentation
  | RSEDocumentationContext
  | RSEDtcStatusChangeNotificationNeeds
  | RSEEcuMapping
  | RSEEocExecutableEntityRef
  | RSEEocExecutableEntityRefAbstract
  | RSEEocExecutableEntityRefGroup
  | RSEEcuAbstractionSwComponentType
  | RSEEcuInstance
  | RSEEcuPartition
  | RSEEcuStateMgrUserNeeds
  | RSEEcuTiming
  | RSEEcucAbstractReferenceDef
  | RSEEcucAbstractStringParamDef
  | RSEEcucAddInfoParamDef
  | RSEEcucBooleanParamDef
  | RSEEcucChoiceContainerDef
  | RSEEcucChoiceReferenceDef
  | RSEEcucCommonAttributes
  | RSEEcucContainerDef
  | RSEEcucContainerValue
  | RSEEcucDefinitionCollection
  | RSEEcucDefinitionElement
  | RSEEcucEnumerationLiteralDef
  | RSEEcucEnumerationParamDef
  | RSEEcucFloatParamDef
  | RSEEcucForeignReferenceDef
  | RSEEcucFunctionNameDef
  | RSEEcucInstanceReferenceDef
  | RSEEcucIntegerParamDef
  | RSEEcucLinkerSymbolDef
  | RSEEcucModuleConfigurationValues
  | RSEEcucModuleDef
  | RSEEcucMultilineStringParamDef
  | RSEEcucParamConfContainerDef
  | RSEEcucParameterDef
  | RSEEcucQuery
  | RSEEcucReferenceDef
  | RSEEcucStringParamDef
  | RSEEcucSymbolicNameReferenceDef
  | RSEEcucValidationCondition
  | RSEEcucValueCollection
  | RSEEndToEndProtection
  | RSEEndToEndProtectionSet
  | RSEEthernetCluster
  | RSEEthernetCommunicationConnector
  | RSEEthernetCommunicationController
  | RSEEthernetFrame
  | RSEEthernetFrameTriggering
  | RSEEthernetPhysicalChannel
  | RSEEvaluatedVariantSet
  | RSEEventHandler
  | RSEEventTriggeringConstraint
  | RSEExclusiveArea
  | RSEExclusiveAreaNestingOrder
  | RSEExecutableEntity
  | RSEExecutableEntityActivationReason
  | RSEExecutionOrderConstraint
  | RSEExecutionTime
  | RSEExecutionTimeConstraint
  | RSEExternalTriggerOccurredEvent
  | RSEExternalTriggeringPointIdent
  | RSEFmAttributeDef
  | RSEFmFeature
  | RSEFmFeatureMap
  | RSEFmFeatureMapAssertion
  | RSEFmFeatureMapCondition
  | RSEFmFeatureMapElement
  | RSEFmFeatureModel
  | RSEFmFeatureRelation
  | RSEFmFeatureRestriction
  | RSEFmFeatureSelection
  | RSEFmFeatureSelectionSet
  | RSEFibexElement
  | RSEFlatInstanceDescriptor
  | RSEFlatMap
  | RSEFlexrayArTpConfig
  | RSEFlexrayArTpNode
  | RSEFlexrayCluster
  | RSEFlexrayCommunicationConnector
  | RSEFlexrayCommunicationController
  | RSEFlexrayFrame
  | RSEFlexrayFrameTriggering
  | RSEFlexrayNmCluster
  | RSEFlexrayNmNode2
  | RSEFlexrayPhysicalChannel
  | RSEFlexrayTpConfig
  | RSEFlexrayTpConnectionControl
  | RSEFlexrayTpNode
  | RSEFlexrayTpPduPool
  | RSEFrame
  | RSEFramePort
  | RSEFrameTriggering
  | RSEFunctionInhibitionNeeds
  | RSEGateway
  | RSEGeneralParameter
  | RSEGeneralPurposeIPdu
  | RSEGeneralPurposePdu
  | RSEHeapUsage
  | RSEHwAttributeDef
  | RSEHwAttributeLiteralDef
  | RSEHwCategory
  | RSEHwDescriptionEntity
  | RSEHwElement
  | RSEHwPin
  | RSEHwPinGroup
  | RSEHwType
  | RSEIPdu
  | RSEIPduPort
  | RSEISignal
  | RSEISignalGroup
  | RSEISignalIPdu
  | RSEISignalIPduGroup
  | RSEISignalPort
  | RSEISignalToIPduMapping
  | RSEISignalTriggering
  | RSEIdentCaption
  | RSEIdentifiable
  | RSEImplementation
  | RSEImplementationDataType
  | RSEImplementationDataTypeElement
  | RSEImplementationProps
  | RSEInitEvent
  | RSEInternalBehavior
  | RSEInternalTriggerOccurredEvent
  | RSEInternalTriggeringPoint
  | RSEInterpolationRoutineMappingSet
  | RSEJ1939Cluster
  | RSEJ1939DcmIPdu
  | RSEJ1939NmCluster
  | RSEJ1939NmNode
  | RSEJ1939TpConfig
  | RSEJ1939TpNode
  | RSEKeyword
  | RSEKeywordSet
  | RSELatencyTimingConstraint
  | RSELifeCycleInfoSet
  | RSELifeCycleState
  | RSELifeCycleStateDefinitionGroup
  | RSELinCluster
  | RSELinCommunicationConnector
  | RSELinCommunicationController
  | RSELinEventTriggeredFrame
  | RSELinFrame
  | RSELinFrameTriggering
  | RSELinMaster
  | RSELinPhysicalChannel
  | RSELinScheduleTable
  | RSELinSlave
  | RSELinSporadicFrame
  | RSELinTpConfig
  | RSELinTpNode
  | RSELinUnconditionalFrame
  | RSELinker
  | RSELogicAddress
  | RSEMacMulticastGroup
  | RSEMcDataInstance
  | RSEMcFunction
  | RSEMeasuredExecutionTime
  | RSEMeasuredHeapUsage
  | RSEMeasuredStackUsage
  | RSEMemorySection
  | RSEModeAccessPointIdent
  | RSEModeDeclaration
  | RSEModeDeclarationGroup
  | RSEModeDeclarationGroupPrototype
  | RSEModeDeclarationMapping
  | RSEModeDeclarationMappingSet
  | RSEModeInterfaceMapping
  | RSEModeSwitchInterface
  | RSEModeSwitchPoint
  | RSEModeSwitchedAckEvent
  | RSEModeTransition
  | RSEMultilanguageReferrable
  | RSEMultiplexedIPdu
  | RSENPdu
  | RSENetworkEndpoint
  | RSENmCluster
  | RSENmConfig
  | RSENmEcu
  | RSENmNode
  | RSENmPdu
  | RSENvBlockDescriptor
  | RSENvBlockNeeds
  | RSENvBlockSwComponentType
  | RSENvDataInterface
  | RSEObdControlServiceNeeds
  | RSEObdInfoServiceNeeds
  | RSEObdMonitorServiceNeeds
  | RSEObdPidServiceNeeds
  | RSEObdRatioServiceNeeds
  | RSEOffsetTimingConstraint
  | RSEOperationInvokedEvent
  | RSEPPortPrototype
  | RSEPrPortPrototype
  | RSEPackageableElement
  | RSEParameterAccess
  | RSEParameterDataPrototype
  | RSEParameterInterface
  | RSEParameterSwComponentType
  | RSEPassThroughSwConnector
  | RSEPdu
  | RSEPduToFrameMapping
  | RSEPduTriggering
  | RSEPdurIPduGroup
  | RSEPerInstanceMemory
  | RSEPeriodicEventTriggering
  | RSEPhysicalChannel
  | RSEPhysicalDimension
  | RSEPhysicalDimensionMappingSet
  | RSEPortGroup
  | RSEPortInterface
  | RSEPortInterfaceMapping
  | RSEPortInterfaceMappingSet
  | RSEPortPrototype
  | RSEPortPrototypeBlueprint
  | RSEPostBuildVariantCriterion
  | RSEPostBuildVariantCriterionValueSet
  | RSEPredefinedVariant
  | RSEProvidedServiceInstance
  | RSERPortPrototype
  | RSERteEvent
  | RSERapidPrototypingScenario
  | RSEReferrable
  | RSEResourceConsumption
  | RSERootSwCompositionPrototype
  | RSERoughEstimateHeapUsage
  | RSERoughEstimateOfExecutionTime
  | RSERoughEstimateStackUsage
  | RSERptContainer
  | RSERunnableEntity
  | RSERunnableEntityGroup
  | RSESdgCaption
  | RSESectionNamePrefix
  | RSESenderReceiverInterface
  | RSESensorActuatorSwComponentType
  | RSESerializationTechnology
  | RSEServerCallPoint
  | RSEServiceNeeds
  | RSEServiceProxySwComponentType
  | RSEServiceSwComponentType
  | RSESimulatedExecutionTime
  | RSESingleLanguageReferrable
  | RSESoAdRoutingGroup
  | RSESocketAddress
  | RSESocketConnectionBundle
  | RSESporadicEventTriggering
  | RSEStackUsage
  | RSEStd
  | RSEStructuredReq
  | RSESupervisedEntityNeeds
  | RSESwAddrMethod
  | RSESwAxisType
  | RSESwBaseType
  | RSESwComponentPrototype
  | RSESwComponentType
  | RSESwConnector
  | RSESwGenericAxisParamType
  | RSESwRecordLayout
  | RSESwServiceArg
  | RSESwSystemconst
  | RSESwSystemconstantValueSet
  | RSESwcBswMapping
  | RSESwcImplementation
  | RSESwcInternalBehavior
  | RSESwcModeManagerErrorEvent
  | RSESwcModeSwitchEvent
  | RSESwcServiceDependency
  | RSESwcTiming
  | RSESwcToEcuMapping
  | RSESwcToImplMapping
  | RSESymbolProps
  | RSESymbolicNameProps
  | RSESyncTimeBaseMgrUserNeeds
  | RSESynchronizationTimingConstraint
  | RSESynchronousServerCallPoint
  | RSESystem
  | RSESystemMapping
  | RSESystemSignal
  | RSESystemSignalGroup
  | RSESystemTiming
  | RSETdEventBsw
  | RSETdEventBswInternalBehavior
  | RSETdEventBswModeDeclaration
  | RSETdEventBswModule
  | RSETdEventCom
  | RSETdEventComplex
  | RSETdEventCycleStart
  | RSETdEventFrClusterCycleStart
  | RSETdEventFrame
  | RSETdEventIPdu
  | RSETdEventISignal
  | RSETdEventModeDeclaration
  | RSETdEventOperation
  | RSETdEventSwc
  | RSETdEventSwcInternalBehavior
  | RSETdEventSwcInternalBehaviorReference
  | RSETdEventTtCanCycleStart
  | RSETdEventTrigger
  | RSETdEventVariableDataPrototype
  | RSETdEventVfb
  | RSETdEventVfbPort
  | RSETdEventVfbReference
  | RSETimeSyncServerConfiguration
  | RSETimingConstraint
  | RSETimingDescription
  | RSETimingDescriptionEvent
  | RSETimingDescriptionEventChain
  | RSETimingEvent
  | RSETimingExtension
  | RSETopic1
  | RSETpAddress
  | RSETpConfig
  | RSETraceReferrable
  | RSETraceable
  | RSETraceableText
  | RSETrigger
  | RSETriggerInterface
  | RSETriggerInterfaceMapping
  | RSETtcanCluster
  | RSETtcanCommunicationConnector
  | RSETtcanCommunicationController
  | RSETtcanPhysicalChannel
  | RSEUdpNmCluster
  | RSEUdpNmNode
  | RSEUnit
  | RSEUnitGroup
  | RSEUserDefinedCluster
  | RSEUserDefinedCommunicationConnector
  | RSEUserDefinedCommunicationController
  | RSEUserDefinedIPdu
  | RSEUserDefinedPdu
  | RSEUserDefinedPhysicalChannel
  | RSEVariableAccess
  | RSEVariableAndParameterInterfaceMapping
  | RSEVariableDataPrototype
  | RSEVariationPointProxy
  | RSEVfbTiming
  | RSEViewMap
  | RSEViewMapSet
  | RSEVlanConfig
  | RSEWaitPoint
  | RSEWarningIndicatorRequestedBitNeeds
  | RSEWorstCaseHeapUsage
  | RSEWorstCaseStackUsage
  | RSEXcpPdu
  | RSEXdoc
  | RSEXfile
  | RSEXrefTarget
  deriving (Show, Eq, Ord)

elementDestType :: Element -> ReferrableSubtypesEnum
--elementDestType (ArElementElement _) = RSEArElement
--elementDestType (ArPackageElement _) = RSEArPackage
--elementDestType (AbstractCanClusterElement _) = RSEAbstractCanCluster
--elementDestType (AbstractCanCommunicationConnectorElement _) = RSEAbstractCanCommunicationConnector
--elementDestType (AbstractCanCommunicationControllerElement _) = RSEAbstractCanCommunicationController
--elementDestType (AbstractCanPhysicalChannelElement _) = RSEAbstractCanPhysicalChannel
--elementDestType (AbstractEventElement _) = RSEAbstractEvent
--elementDestType (AbstractProvidedPortPrototypeElement _) = RSEAbstractProvidedPortPrototype
--elementDestType (AbstractRequiredPortPrototypeElement _) = RSEAbstractRequiredPortPrototype
--elementDestType (AbstractServiceInstanceElement _) = RSEAbstractServiceInstance
--elementDestType (AclObjectSetElement _) = RSEAclObjectSet
--elementDestType (AclOperationElement _) = RSEAclOperation
--elementDestType (AclPermissionElement _) = RSEAclPermission
--elementDestType (AclRoleElement _) = RSEAclRole
--elementDestType (AgeConstraintElement _) = RSEAgeConstraint
--elementDestType (AliasNameSetElement _) = RSEAliasNameSet
--elementDestType (AnalyzedExecutionTimeElement _) = RSEAnalyzedExecutionTime
--elementDestType (ApplicationArrayDataTypeElement _) = RSEApplicationArrayDataType
--elementDestType (ApplicationArrayElementElement _) = RSEApplicationArrayElement
--elementDestType (ApplicationCompositeDataTypeElement _) = RSEApplicationCompositeDataType
--elementDestType (ApplicationCompositeElementDataPrototypeElement _) = RSEApplicationCompositeElementDataPrototype
--elementDestType (ApplicationDataTypeElement _) = RSEApplicationDataType
--elementDestType (ApplicationEndpointElement _) = RSEApplicationEndpoint
--elementDestType (ApplicationErrorElement _) = RSEApplicationError
--elementDestType (ApplicationPrimitiveDataTypeElement _) = RSEApplicationPrimitiveDataType
--elementDestType (ApplicationRecordDataTypeElement _) = RSEApplicationRecordDataType
--elementDestType (ApplicationRecordElementElement _) = RSEApplicationRecordElement
elementDestType (ApplicationSwComponentTypeElement _) = RSEApplicationSwComponentType
--elementDestType (ArbitraryEventTriggeringElement _) = RSEArbitraryEventTriggering
elementDestType (ArgumentDataPrototypeElement _) = RSEArgumentDataPrototype
elementDestType (SwConnectorElement (AssemblySwConnector {})) = RSEAssemblySwConnector
--elementDestType (AsynchronousServerCallPointElement _) = RSEAsynchronousServerCallPoint
elementDestType (AsynchronousServerCallResultPointElement _) = RSEAsynchronousServerCallResultPoint
-- elementDestType (AsynchronousServerCallReturnsEventElement _) = RSEAsynchronousServerCallReturnsEvent
-- abtract: elementDestType (AtomicSwComponentTypeElement _) = RSEAtomicSwComponentType
--elementDestType (AtpBlueprintElement _) = RSEAtpBlueprint
--elementDestType (AtpBlueprintableElement _) = RSEAtpBlueprintable
--elementDestType (AtpClassifierElement _) = RSEAtpClassifier
--elementDestType (AtpDefinitionElement _) = RSEAtpDefinition
--elementDestType (AtpFeatureElement _) = RSEAtpFeature
--elementDestType (AtpPrototypeElement _) = RSEAtpPrototype
--elementDestType (AtpStructureElementElement _) = RSEAtpStructureElement
--elementDestType (AtpTypeElement _) = RSEAtpType
--elementDestType (AutosarDataPrototypeElement _) = RSEAutosarDataPrototype
--elementDestType (AutosarDataTypeElement _) = RSEAutosarDataType
--elementDestType (AutosarOperationArgumentInstanceElement _) = RSEAutosarOperationArgumentInstance
--elementDestType (AutosarVariableInstanceElement _) = RSEAutosarVariableInstance
--elementDestType (BackgroundEventElement _) = RSEBackgroundEvent
--elementDestType (BaseTypeElement _) = RSEBaseType
--elementDestType (BlueprintMappingSetElement _) = RSEBlueprintMappingSet
--elementDestType (BswAsynchronousServerCallPointElement _) = RSEBswAsynchronousServerCallPoint
--elementDestType (BswAsynchronousServerCallResultPointElement _) = RSEBswAsynchronousServerCallResultPoint
--elementDestType (BswAsynchronousServerCallReturnsEventElement _) = RSEBswAsynchronousServerCallReturnsEvent
--elementDestType (BswBackgroundEventElement _) = RSEBswBackgroundEvent
--elementDestType (BswCalledEntityElement _) = RSEBswCalledEntity
--elementDestType (BswDataReceivedEventElement _) = RSEBswDataReceivedEvent
--elementDestType (BswDebugInfoElement _) = RSEBswDebugInfo
--elementDestType (BswDirectCallPointElement _) = RSEBswDirectCallPoint
--elementDestType (BswDistinguishedPartitionElement _) = RSEBswDistinguishedPartition
--elementDestType (BswEventElement _) = RSEBswEvent
--elementDestType (BswExternalTriggerOccurredEventElement _) = RSEBswExternalTriggerOccurredEvent
--elementDestType (ImplementationElement _) = RSEBswImplementation
--elementDestType (BswInternalBehaviorElement _) = RSEBswInternalBehavior
--elementDestType (BswInternalTriggerOccurredEventElement _) = RSEBswInternalTriggerOccurredEvent
--elementDestType (BswInternalTriggeringPointElement _) = RSEBswInternalTriggeringPoint
--elementDestType (BswInterruptEntityElement _) = RSEBswInterruptEntity
--elementDestType (BswMgrNeedsElement _) = RSEBswMgrNeeds
--elementDestType (BswModeManagerErrorEventElement _) = RSEBswModeManagerErrorEvent
--elementDestType (BswModeSwitchEventElement _) = RSEBswModeSwitchEvent
--elementDestType (BswModeSwitchedAckEventElement _) = RSEBswModeSwitchedAckEvent
--elementDestType (BswModuleCallPointElement _) = RSEBswModuleCallPoint
--elementDestType (BswModuleClientServerEntryElement _) = RSEBswModuleClientServerEntry
--elementDestType (BswModuleDependencyElement _) = RSEBswModuleDependency
--elementDestType (BswModuleDescriptionElement _) = RSEBswModuleDescription
--elementDestType (BswModuleEntityElement _) = RSEBswModuleEntity
--elementDestType (BswModuleEntryElement _) = RSEBswModuleEntry
--elementDestType (BswModuleTimingElement _) = RSEBswModuleTiming
--elementDestType (BswOperationInvokedEventElement _) = RSEBswOperationInvokedEvent
--elementDestType (BswSchedulableEntityElement _) = RSEBswSchedulableEntity
--elementDestType (BswScheduleEventElement _) = RSEBswScheduleEvent
--elementDestType (BswSchedulerNamePrefixElement _) = RSEBswSchedulerNamePrefix
--elementDestType (BswSynchronousServerCallPointElement _) = RSEBswSynchronousServerCallPoint
--elementDestType (BswTimingEventElement _) = RSEBswTimingEvent
--elementDestType (BswVariableAccessElement _) = RSEBswVariableAccess
--elementDestType (BuildActionElement _) = RSEBuildAction
--elementDestType (BuildActionEntityElement _) = RSEBuildActionEntity
--elementDestType (BuildActionEnvironmentElement _) = RSEBuildActionEnvironment
--elementDestType (BuildActionManifestElement _) = RSEBuildActionManifest
--elementDestType (BurstPatternEventTriggeringElement _) = RSEBurstPatternEventTriggering
elementDestType (CalibrationParameterValueSetElement _) = RSECalibrationParameterValueSet
--elementDestType (CanClusterElement _) = RSECanCluster
--elementDestType (CanCommunicationConnectorElement _) = RSECanCommunicationConnector
--elementDestType (CanCommunicationControllerElement _) = RSECanCommunicationController
--elementDestType (CanFrameElement _) = RSECanFrame
--elementDestType (CanFrameTriggeringElement _) = RSECanFrameTriggering
--elementDestType (CanNmClusterElement _) = RSECanNmCluster
--elementDestType (CanNmNodeElement _) = RSECanNmNode
--elementDestType (CanPhysicalChannelElement _) = RSECanPhysicalChannel
--elementDestType (CanTpAddressElement _) = RSECanTpAddress
--elementDestType (CanTpChannelElement _) = RSECanTpChannel
--elementDestType (CanTpConfigElement _) = RSECanTpConfig
--elementDestType (CanTpNodeElement _) = RSECanTpNode
--elementDestType (CaptionElement _) = RSECaption
--elementDestType (ChapterElement _) = RSEChapter
elementDestType (ClientServerInterfaceElement _) = RSEClientServerInterface
-- elementDestType (ClientServerInterfaceMappingElement _) = RSEClientServerInterfaceMapping
elementDestType (ClientServerOperationElement _) = RSEClientServerOperation
--elementDestType (CodeElement _) = RSECode
--elementDestType (CollectableElementElement _) = RSECollectableElement
--elementDestType (CollectionElement _) = RSECollection
--elementDestType (ComMgrUserNeedsElement _) = RSEComMgrUserNeeds
--elementDestType (CommConnectorPortElement _) = RSECommConnectorPort
--elementDestType (CommunicationClusterElement _) = RSECommunicationCluster
--elementDestType (CommunicationConnectorElement _) = RSECommunicationConnector
--elementDestType (CommunicationControllerElement _) = RSECommunicationController
--elementDestType (CompilerElement _) = RSECompiler
elementDestType (ComplexDeviceDriverSwComponentTypeElement _) = RSEComplexDeviceDriverSwComponentType
elementDestType (CompositionSwComponentTypeElement _) = RSECompositionSwComponentType
--elementDestType (CompuMethodElement _) = RSECompuMethod
--elementDestType (ConcretePatternEventTriggeringElement _) = RSEConcretePatternEventTriggering
elementDestType (ConsistencyNeedsElement _) = RSEConsistencyNeeds
--elementDestType (ConsistencyNeedsBlueprintSetElement _) = RSEConsistencyNeedsBlueprintSet
--elementDestType (ConstantSpecificationElement _) = RSEConstantSpecification
elementDestType (ConstantSpecificationMappingSetElement _) = RSEConstantSpecificationMappingSet
--elementDestType (ConsumedEventGroupElement _) = RSEConsumedEventGroup
--elementDestType (ConsumedServiceInstanceElement _) = RSEConsumedServiceInstance
--elementDestType (CouplingElementElement _) = RSECouplingElement
--elementDestType (CouplingPortElement _) = RSECouplingPort
--elementDestType (CryptoServiceNeedsElement _) = RSECryptoServiceNeeds
--elementDestType (DataConstrElement _) = RSEDataConstr
--elementDestType (DataInterfaceElement _) = RSEDataInterface
--elementDestType (DataPrototypeElement _) = RSEDataPrototype
--elementDestType (DataPrototypeGroupElement _) = RSEDataPrototypeGroup
--elementDestType (DataReceiveErrorEventElement _) = RSEDataReceiveErrorEvent
--elementDestType (DataReceivedEventElement _) = RSEDataReceivedEvent
--elementDestType (DataSendCompletedEventElement _) = RSEDataSendCompletedEvent
elementDestType (DataTypeMappingSetElement _) = RSEDataTypeMappingSet
--elementDestType (DataWriteCompletedEventElement _) = RSEDataWriteCompletedEvent
--elementDestType (DcmIPduElement _) = RSEDcmIPdu
--elementDestType (DefItemElement _) = RSEDefItem
elementDestType (SwConnectorElement (DelegationSwConnector {})) = RSEDelegationSwConnector
--elementDestType (DependencyOnArtifactElement _) = RSEDependencyOnArtifact
--elementDestType (DiagEventDebounceAlgorithmElement _) = RSEDiagEventDebounceAlgorithm
--elementDestType (DiagEventDebounceCounterBasedElement _) = RSEDiagEventDebounceCounterBased
--elementDestType (DiagEventDebounceMonitorInternalElement _) = RSEDiagEventDebounceMonitorInternal
--elementDestType (DiagEventDebounceTimeBasedElement _) = RSEDiagEventDebounceTimeBased
--elementDestType (DiagnosticCapabilityElementElement _) = RSEDiagnosticCapabilityElement
--elementDestType (DiagnosticCommunicationManagerNeedsElement _) = RSEDiagnosticCommunicationManagerNeeds
--elementDestType (DiagnosticEnableConditionNeedsElement _) = RSEDiagnosticEnableConditionNeeds
--elementDestType (DiagnosticEventInfoNeedsElement _) = RSEDiagnosticEventInfoNeeds
--elementDestType (DiagnosticEventManagerNeedsElement _) = RSEDiagnosticEventManagerNeeds
--elementDestType (DiagnosticEventNeedsElement _) = RSEDiagnosticEventNeeds
--elementDestType (DiagnosticIoControlNeedsElement _) = RSEDiagnosticIoControlNeeds
--elementDestType (DiagnosticOperationCycleNeedsElement _) = RSEDiagnosticOperationCycleNeeds
--elementDestType (DiagnosticRoutineNeedsElement _) = RSEDiagnosticRoutineNeeds
--elementDestType (DiagnosticStorageConditionNeedsElement _) = RSEDiagnosticStorageConditionNeeds
--elementDestType (DiagnosticValueNeedsElement _) = RSEDiagnosticValueNeeds
--elementDestType (DiagnosticsCommunicationSecurityNeedsElement _) = RSEDiagnosticsCommunicationSecurityNeeds
--elementDestType (DltUserNeedsElement _) = RSEDltUserNeeds
--elementDestType (DoIpActivationLineNeedsElement _) = RSEDoIpActivationLineNeeds
--elementDestType (DoIpGidNeedsElement _) = RSEDoIpGidNeeds
--elementDestType (DoIpGidSynchronizationNeedsElement _) = RSEDoIpGidSynchronizationNeeds
--elementDestType (DoIpPowerModeStatusNeedsElement _) = RSEDoIpPowerModeStatusNeeds
--elementDestType (DoIpRoutingActivationAuthenticationNeedsElement _) = RSEDoIpRoutingActivationAuthenticationNeeds
--elementDestType (DoIpRoutingActivationConfirmationNeedsElement _) = RSEDoIpRoutingActivationConfirmationNeeds
--elementDestType (DoIpServiceNeedsElement _) = RSEDoIpServiceNeeds
--elementDestType (DocumentationElement _) = RSEDocumentation
--elementDestType (DocumentationContextElement _) = RSEDocumentationContext
--elementDestType (DtcStatusChangeNotificationNeedsElement _) = RSEDtcStatusChangeNotificationNeeds
elementDestType (EcuMappingElement _) = RSEEcuMapping
--elementDestType (EocExecutableEntityRefElement _) = RSEEocExecutableEntityRef
--elementDestType (EocExecutableEntityRefAbstractElement _) = RSEEocExecutableEntityRefAbstract
--elementDestType (EocExecutableEntityRefGroupElement _) = RSEEocExecutableEntityRefGroup
elementDestType (EcuAbstractionSwComponentTypeElement _) = RSEEcuAbstractionSwComponentType
elementDestType (EcuInstanceElement _) = RSEEcuInstance
--elementDestType (EcuPartitionElement _) = RSEEcuPartition
--elementDestType (EcuStateMgrUserNeedsElement _) = RSEEcuStateMgrUserNeeds
--elementDestType (EcuTimingElement _) = RSEEcuTiming
--elementDestType (EcucAbstractReferenceDefElement _) = RSEEcucAbstractReferenceDef
--elementDestType (EcucAbstractStringParamDefElement _) = RSEEcucAbstractStringParamDef
--elementDestType (EcucAddInfoParamDefElement _) = RSEEcucAddInfoParamDef
--elementDestType (EcucBooleanParamDefElement _) = RSEEcucBooleanParamDef
--elementDestType (EcucChoiceContainerDefElement _) = RSEEcucChoiceContainerDef
--elementDestType (EcucChoiceReferenceDefElement _) = RSEEcucChoiceReferenceDef
--elementDestType (EcucCommonAttributesElement _) = RSEEcucCommonAttributes
--elementDestType (EcucContainerDefElement _) = RSEEcucContainerDef
--elementDestType (EcucContainerValueElement _) = RSEEcucContainerValue
--elementDestType (EcucDefinitionCollectionElement _) = RSEEcucDefinitionCollection
--elementDestType (EcucDefinitionElementElement _) = RSEEcucDefinitionElement
--elementDestType (EcucEnumerationLiteralDefElement _) = RSEEcucEnumerationLiteralDef
--elementDestType (EcucEnumerationParamDefElement _) = RSEEcucEnumerationParamDef
--elementDestType (EcucFloatParamDefElement _) = RSEEcucFloatParamDef
--elementDestType (EcucForeignReferenceDefElement _) = RSEEcucForeignReferenceDef
--elementDestType (EcucFunctionNameDefElement _) = RSEEcucFunctionNameDef
--elementDestType (EcucInstanceReferenceDefElement _) = RSEEcucInstanceReferenceDef
--elementDestType (EcucIntegerParamDefElement _) = RSEEcucIntegerParamDef
--elementDestType (EcucLinkerSymbolDefElement _) = RSEEcucLinkerSymbolDef
--elementDestType (EcucModuleConfigurationValuesElement _) = RSEEcucModuleConfigurationValues
--elementDestType (EcucModuleDefElement _) = RSEEcucModuleDef
--elementDestType (EcucMultilineStringParamDefElement _) = RSEEcucMultilineStringParamDef
--elementDestType (EcucParamConfContainerDefElement _) = RSEEcucParamConfContainerDef
-- elementDestType (EcucParameterDefElement _) = RSEEcucParameterDef
-- elementDestType (EcucQueryElement _) = RSEEcucQuery
-- elementDestType (EcucReferenceDefElement _) = RSEEcucReferenceDef
-- elementDestType (EcucStringParamDefElement _) = RSEEcucStringParamDef
-- elementDestType (EcucSymbolicNameReferenceDefElement _) = RSEEcucSymbolicNameReferenceDef
-- elementDestType (EcucValidationConditionElement _) = RSEEcucValidationCondition
-- elementDestType (EcucValueCollectionElement _) = RSEEcucValueCollection
-- elementDestType (EndToEndProtectionElement _) = RSEEndToEndProtection
-- elementDestType (EndToEndProtectionSetElement _) = RSEEndToEndProtectionSet
-- elementDestType (EthernetClusterElement _) = RSEEthernetCluster
-- elementDestType (EthernetCommunicationConnectorElement _) = RSEEthernetCommunicationConnector
-- elementDestType (EthernetCommunicationControllerElement _) = RSEEthernetCommunicationController
-- elementDestType (EthernetFrameElement _) = RSEEthernetFrame
-- elementDestType (EthernetFrameTriggeringElement _) = RSEEthernetFrameTriggering
-- elementDestType (EthernetPhysicalChannelElement _) = RSEEthernetPhysicalChannel
-- elementDestType (EvaluatedVariantSetElement _) = RSEEvaluatedVariantSet
-- elementDestType (EventHandlerElement _) = RSEEventHandler
-- elementDestType (EventTriggeringConstraintElement _) = RSEEventTriggeringConstraint
-- elementDestType (ExclusiveAreaElement _) = RSEExclusiveArea
-- elementDestType (ExclusiveAreaNestingOrderElement _) = RSEExclusiveAreaNestingOrder
-- elementDestType (ExecutableEntityElement _) = RSEExecutableEntity
-- elementDestType (ExecutableEntityActivationReasonElement _) = RSEExecutableEntityActivationReason
-- elementDestType (ExecutionOrderConstraintElement _) = RSEExecutionOrderConstraint
-- elementDestType (ExecutionTimeElement _) = RSEExecutionTime
-- elementDestType (ExecutionTimeConstraintElement _) = RSEExecutionTimeConstraint
-- elementDestType (ExternalTriggerOccurredEventElement _) = RSEExternalTriggerOccurredEvent
-- elementDestType (ExternalTriggeringPointIdentElement _) = RSEExternalTriggeringPointIdent
-- elementDestType (FmAttributeDefElement _) = RSEFmAttributeDef
-- elementDestType (FmFeatureElement _) = RSEFmFeature
-- elementDestType (FmFeatureMapElement _) = RSEFmFeatureMap
-- elementDestType (FmFeatureMapAssertionElement _) = RSEFmFeatureMapAssertion
-- elementDestType (FmFeatureMapConditionElement _) = RSEFmFeatureMapCondition
-- elementDestType (FmFeatureMapElementElement _) = RSEFmFeatureMapElement
-- elementDestType (FmFeatureModelElement _) = RSEFmFeatureModel
-- elementDestType (FmFeatureRelationElement _) = RSEFmFeatureRelation
-- elementDestType (FmFeatureRestrictionElement _) = RSEFmFeatureRestriction
-- elementDestType (FmFeatureSelectionElement _) = RSEFmFeatureSelection
-- elementDestType (FmFeatureSelectionSetElement _) = RSEFmFeatureSelectionSet
elementDestType (FibexElementElement _) = RSEFibexElement
-- elementDestType (FlatInstanceDescriptorElement _) = RSEFlatInstanceDescriptor
elementDestType (FlatMapElement _) = RSEFlatMap
-- elementDestType (FlexrayArTpConfigElement _) = RSEFlexrayArTpConfig
-- elementDestType (FlexrayArTpNodeElement _) = RSEFlexrayArTpNode
-- elementDestType (FlexrayClusterElement _) = RSEFlexrayCluster
-- elementDestType (FlexrayCommunicationConnectorElement _) = RSEFlexrayCommunicationConnector
-- elementDestType (FlexrayCommunicationControllerElement _) = RSEFlexrayCommunicationController
-- elementDestType (FlexrayFrameElement _) = RSEFlexrayFrame
-- elementDestType (FlexrayFrameTriggeringElement _) = RSEFlexrayFrameTriggering
-- elementDestType (FlexrayNmClusterElement _) = RSEFlexrayNmCluster
-- elementDestType (FlexrayNmNode2Element _) = RSEFlexrayNmNode2
-- elementDestType (FlexrayPhysicalChannelElement _) = RSEFlexrayPhysicalChannel
-- elementDestType (FlexrayTpConfigElement _) = RSEFlexrayTpConfig
-- elementDestType (FlexrayTpConnectionControlElement _) = RSEFlexrayTpConnectionControl
-- elementDestType (FlexrayTpNodeElement _) = RSEFlexrayTpNode
-- elementDestType (FlexrayTpPduPoolElement _) = RSEFlexrayTpPduPool
-- elementDestType (FrameElement _) = RSEFrame
-- elementDestType (FramePortElement _) = RSEFramePort
-- elementDestType (FrameTriggeringElement _) = RSEFrameTriggering
-- elementDestType (FunctionInhibitionNeedsElement _) = RSEFunctionInhibitionNeeds
-- elementDestType (GatewayElement _) = RSEGateway
-- elementDestType (GeneralParameterElement _) = RSEGeneralParameter
-- elementDestType (GeneralPurposeIPduElement _) = RSEGeneralPurposeIPdu
-- elementDestType (GeneralPurposePduElement _) = RSEGeneralPurposePdu
-- elementDestType (HeapUsageElement _) = RSEHeapUsage
-- elementDestType (HwAttributeDefElement _) = RSEHwAttributeDef
-- elementDestType (HwAttributeLiteralDefElement _) = RSEHwAttributeLiteralDef
-- elementDestType (HwCategoryElement _) = RSEHwCategory
-- elementDestType (HwDescriptionEntityElement _) = RSEHwDescriptionEntity
-- elementDestType (HwElementElement _) = RSEHwElement
-- elementDestType (HwPinElement _) = RSEHwPin
-- elementDestType (HwPinGroupElement _) = RSEHwPinGroup
-- elementDestType (HwTypeElement _) = RSEHwType
-- elementDestType (IPduElement _) = RSEIPdu
-- elementDestType (IPduPortElement _) = RSEIPduPort
-- elementDestType (ISignalElement _) = RSEISignal
-- elementDestType (ISignalGroupElement _) = RSEISignalGroup
-- elementDestType (ISignalIPduElement _) = RSEISignalIPdu
-- elementDestType (ISignalIPduGroupElement _) = RSEISignalIPduGroup
-- elementDestType (ISignalPortElement _) = RSEISignalPort
-- elementDestType (ISignalToIPduMappingElement _) = RSEISignalToIPduMapping
-- elementDestType (ISignalTriggeringElement _) = RSEISignalTriggering
-- elementDestType (IdentCaptionElement _) = RSEIdentCaption
-- elementDestType (IdentifiableElement _) = RSEIdentifiable
-- elementDestType (ImplementationElement _) = RSEImplementation
-- elementDestType (ImplementationDataTypeElement _) = RSEImplementationDataType
-- elementDestType (ImplementationDataTypeElementElement _) = RSEImplementationDataTypeElement
-- elementDestType (ImplementationPropsElement _) = RSEImplementationProps
-- elementDestType (InitEventElement _) = RSEInitEvent
-- elementDestType (InternalBehaviorElement _) = RSEInternalBehavior
-- elementDestType (InternalTriggerOccurredEventElement _) = RSEInternalTriggerOccurredEvent
-- elementDestType (InternalTriggeringPointElement _) = RSEInternalTriggeringPoint
-- elementDestType (InterpolationRoutineMappingSetElement _) = RSEInterpolationRoutineMappingSet
-- elementDestType (J1939ClusterElement _) = RSEJ1939Cluster
-- elementDestType (J1939DcmIPduElement _) = RSEJ1939DcmIPdu
-- elementDestType (J1939NmClusterElement _) = RSEJ1939NmCluster
-- elementDestType (J1939NmNodeElement _) = RSEJ1939NmNode
-- elementDestType (J1939TpConfigElement _) = RSEJ1939TpConfig
-- elementDestType (J1939TpNodeElement _) = RSEJ1939TpNode
-- elementDestType (KeywordElement _) = RSEKeyword
-- elementDestType (KeywordSetElement _) = RSEKeywordSet
-- elementDestType (LatencyTimingConstraintElement _) = RSELatencyTimingConstraint
-- elementDestType (LifeCycleInfoSetElement _) = RSELifeCycleInfoSet
-- elementDestType (LifeCycleStateElement _) = RSELifeCycleState
-- elementDestType (LifeCycleStateDefinitionGroupElement _) = RSELifeCycleStateDefinitionGroup
-- elementDestType (LinClusterElement _) = RSELinCluster
-- elementDestType (LinCommunicationConnectorElement _) = RSELinCommunicationConnector
-- elementDestType (LinCommunicationControllerElement _) = RSELinCommunicationController
-- elementDestType (LinEventTriggeredFrameElement _) = RSELinEventTriggeredFrame
-- elementDestType (LinFrameElement _) = RSELinFrame
-- elementDestType (LinFrameTriggeringElement _) = RSELinFrameTriggering
-- elementDestType (LinMasterElement _) = RSELinMaster
-- elementDestType (LinPhysicalChannelElement _) = RSELinPhysicalChannel
-- elementDestType (LinScheduleTableElement _) = RSELinScheduleTable
-- elementDestType (LinSlaveElement _) = RSELinSlave
-- elementDestType (LinSporadicFrameElement _) = RSELinSporadicFrame
-- elementDestType (LinTpConfigElement _) = RSELinTpConfig
-- elementDestType (LinTpNodeElement _) = RSELinTpNode
-- elementDestType (LinUnconditionalFrameElement _) = RSELinUnconditionalFrame
-- elementDestType (LinkerElement _) = RSELinker
-- elementDestType (LogicAddressElement _) = RSELogicAddress
-- elementDestType (MacMulticastGroupElement _) = RSEMacMulticastGroup
-- elementDestType (McDataInstanceElement _) = RSEMcDataInstance
-- elementDestType (McFunctionElement _) = RSEMcFunction
-- elementDestType (MeasuredExecutionTimeElement _) = RSEMeasuredExecutionTime
-- elementDestType (MeasuredHeapUsageElement _) = RSEMeasuredHeapUsage
-- elementDestType (MeasuredStackUsageElement _) = RSEMeasuredStackUsage
-- elementDestType (MemorySectionElement _) = RSEMemorySection
-- elementDestType (ModeAccessPointIdentElement _) = RSEModeAccessPointIdent
-- elementDestType (ModeDeclarationElement _) = RSEModeDeclaration
-- elementDestType (ModeDeclarationGroupElement _) = RSEModeDeclarationGroup
elementDestType (ModeDeclarationGroupPrototypeElement _) = RSEModeDeclarationGroupPrototype
-- elementDestType (ModeDeclarationMappingElement _) = RSEModeDeclarationMapping
-- elementDestType (ModeDeclarationMappingSetElement _) = RSEModeDeclarationMappingSet
-- elementDestType (ModeInterfaceMappingElement _) = RSEModeInterfaceMapping
elementDestType (ModeSwitchInterfaceElement _) = RSEModeSwitchInterface
elementDestType (ModeSwitchPointElement _) = RSEModeSwitchPoint
-- elementDestType (ModeSwitchedAckEventElement _) = RSEModeSwitchedAckEvent
-- elementDestType (ModeTransitionElement _) = RSEModeTransition
-- elementDestType (MultilanguageReferrableElement _) = RSEMultilanguageReferrable
-- elementDestType (MultiplexedIPduElement _) = RSEMultiplexedIPdu
-- elementDestType (NPduElement _) = RSENPdu
-- elementDestType (NetworkEndpointElement _) = RSENetworkEndpoint
-- elementDestType (NmClusterElement _) = RSENmCluster
-- elementDestType (NmConfigElement _) = RSENmConfig
-- elementDestType (NmEcuElement _) = RSENmEcu
-- elementDestType (NmNodeElement _) = RSENmNode
-- elementDestType (NmPduElement _) = RSENmPdu
elementDestType (NvBlockDescriptorElement _) = RSENvBlockDescriptor
--elementDestType (NvBlockNeedsElement _) = RSENvBlockNeeds
elementDestType (NvBlockSwComponentTypeElement _) = RSENvBlockSwComponentType
elementDestType (NvDataInterfaceElement _) = RSENvDataInterface
-- elementDestType (ObdControlServiceNeedsElement _) = RSEObdControlServiceNeeds
-- elementDestType (ObdInfoServiceNeedsElement _) = RSEObdInfoServiceNeeds
-- elementDestType (ObdMonitorServiceNeedsElement _) = RSEObdMonitorServiceNeeds
-- elementDestType (ObdPidServiceNeedsElement _) = RSEObdPidServiceNeeds
-- elementDestType (ObdRatioServiceNeedsElement _) = RSEObdRatioServiceNeeds
-- elementDestType (OffsetTimingConstraintElement _) = RSEOffsetTimingConstraint
-- elementDestType (OperationInvokedEventElement _) = RSEOperationInvokedEvent
elementDestType (PortPrototypeElement (PPortPrototype {})) = RSEPPortPrototype
elementDestType (PortPrototypeElement (PRPortPrototype {})) = RSEPrPortPrototype
elementDestType (PortPrototypeElement (RPortPrototype {})) = RSERPortPrototype
-- elementDestType (PackageableElementElement _) = RSEPackageableElement
elementDestType (ParameterAccessElement _) = RSEParameterAccess
elementDestType (ParameterDataPrototypeElement _) = RSEParameterDataPrototype
elementDestType (ParameterInterfaceElement _) = RSEParameterInterface
elementDestType (ParameterSwComponentTypeElement _) = RSEParameterSwComponentType
elementDestType (SwConnectorElement (PassThroughSwConnector {})) = RSEPassThroughSwConnector
-- elementDestType (PduElement _) = RSEPdu
-- elementDestType (PduToFrameMappingElement _) = RSEPduToFrameMapping
-- elementDestType (PduTriggeringElement _) = RSEPduTriggering
-- elementDestType (PdurIPduGroupElement _) = RSEPdurIPduGroup
elementDestType (PerInstanceMemoryElement _) = RSEPerInstanceMemory
-- elementDestType (PeriodicEventTriggeringElement _) = RSEPeriodicEventTriggering
-- elementDestType (PhysicalChannelElement _) = RSEPhysicalChannel
-- elementDestType (PhysicalDimensionElement _) = RSEPhysicalDimension
-- elementDestType (PhysicalDimensionMappingSetElement _) = RSEPhysicalDimensionMappingSet
elementDestType (PortGroupElement _) = RSEPortGroup
-- abstract elementDestType (PortInterfaceElement _) = RSEPortInterface
-- abstract TODO elementDestType (PortInterfaceMappingElement _) = RSEPortInterfaceMapping
elementDestType (PortInterfaceMappingSetElement _) = RSEPortInterfaceMappingSet
-- abstract elementDestType (PortPrototypeElement _) = RSEPortPrototype
-- elementDestType (PortPrototypeBlueprintElement _) = RSEPortPrototypeBlueprint
-- elementDestType (PostBuildVariantCriterionElement _) = RSEPostBuildVariantCriterion
-- elementDestType (PostBuildVariantCriterionValueSetElement _) = RSEPostBuildVariantCriterionValueSet
-- elementDestType (PredefinedVariantElement _) = RSEPredefinedVariant
-- elementDestType (ProvidedServiceInstanceElement _) = RSEProvidedServiceInstance
-- elementDestType (RteEventElement _) = RSERteEvent
-- elementDestType (RapidPrototypingScenarioElement _) = RSERapidPrototypingScenario
-- elementDestType (ReferrableElement _) = RSEReferrable
-- elementDestType (ResourceConsumptionElement _) = RSEResourceConsumption
elementDestType (RootSwCompositionPrototypeElement _) = RSERootSwCompositionPrototype
-- elementDestType (RoughEstimateHeapUsageElement _) = RSERoughEstimateHeapUsage
-- elementDestType (RoughEstimateOfExecutionTimeElement _) = RSERoughEstimateOfExecutionTime
-- elementDestType (RoughEstimateStackUsageElement _) = RSERoughEstimateStackUsage
-- elementDestType (RptContainerElement _) = RSERptContainer
elementDestType (RunnableEntityElement _) = RSERunnableEntity
-- elementDestType (RunnableEntityGroupElement _) = RSERunnableEntityGroup
-- elementDestType (SdgCaptionElement _) = RSESdgCaption
-- elementDestType (SectionNamePrefixElement _) = RSESectionNamePrefix
elementDestType (SenderReceiverInterfaceElement _) = RSESenderReceiverInterface
elementDestType (SensorActuatorSwComponentTypeElement _) = RSESensorActuatorSwComponentType
-- elementDestType (SerializationTechnologyElement _) = RSESerializationTechnology
-- elementDestType (ServerCallPointElement _) = RSEServerCallPoint
-- elementDestType (ServiceNeedsElement _) = RSEServiceNeeds
elementDestType (ServiceProxySwComponentTypeElement _) = RSEServiceProxySwComponentType
elementDestType (ServiceSwComponentTypeElement _) = RSEServiceSwComponentType
-- elementDestType (SimulatedExecutionTimeElement _) = RSESimulatedExecutionTime
-- elementDestType (SingleLanguageReferrableElement _) = RSESingleLanguageReferrable
-- elementDestType (SoAdRoutingGroupElement _) = RSESoAdRoutingGroup
-- elementDestType (SocketAddressElement _) = RSESocketAddress
-- elementDestType (SocketConnectionBundleElement _) = RSESocketConnectionBundle
-- elementDestType (SporadicEventTriggeringElement _) = RSESporadicEventTriggering
-- elementDestType (StackUsageElement _) = RSEStackUsage
-- elementDestType (StdElement _) = RSEStd
-- elementDestType (StructuredReqElement _) = RSEStructuredReq
-- elementDestType (SupervisedEntityNeedsElement _) = RSESupervisedEntityNeeds
-- elementDestType (SwAddrMethodElement _) = RSESwAddrMethod
-- elementDestType (SwAxisTypeElement _) = RSESwAxisType
-- elementDestType (SwBaseTypeElement _) = RSESwBaseType
elementDestType (SwComponentPrototypeElement _) = RSESwComponentPrototype
-- abstract: elementDestType (SwComponentTypeElement _) = RSESwComponentType
-- abstract/remove: elementDestType (SwConnectorElement _) = RSESwConnector
-- elementDestType (SwGenericAxisParamTypeElement _) = RSESwGenericAxisParamType
-- elementDestType (SwRecordLayoutElement _) = RSESwRecordLayout
-- elementDestType (SwServiceArgElement _) = RSESwServiceArg
elementDestType (SwSystemconstElement _) = RSESwSystemconst
-- elementDestType (SwSystemconstantValueSetElement _) = RSESwSystemconstantValueSet
elementDestType (SwcBswMappingElement _) = RSESwcBswMapping
elementDestType (SwcImplementationElement _) = RSESwcImplementation
elementDestType (SwcInternalBehaviorElement _) = RSESwcInternalBehavior
-- elementDestType (SwcModeManagerErrorEventElement _) = RSESwcModeManagerErrorEvent
-- elementDestType (SwcModeSwitchEventElement _) = RSESwcModeSwitchEvent
-- elementDestType (SwcServiceDependencyElement _) = RSESwcServiceDependency
-- elementDestType (SwcTimingElement _) = RSESwcTiming
elementDestType (SwcToEcuMappingElement _) = RSESwcToEcuMapping
elementDestType (SwcToImplMappingElement _) = RSESwcToImplMapping
-- elementDestType (SymbolPropsElement _) = RSESymbolProps
-- elementDestType (SymbolicNamePropsElement _) = RSESymbolicNameProps
-- elementDestType (SyncTimeBaseMgrUserNeedsElement _) = RSESyncTimeBaseMgrUserNeeds
-- elementDestType (SynchronizationTimingConstraintElement _) = RSESynchronizationTimingConstraint
-- elementDestType (SynchronousServerCallPointElement _) = RSESynchronousServerCallPoint
elementDestType (SystemElement _) = RSESystem
elementDestType (SystemMappingElement _) = RSESystemMapping
elementDestType (SystemSignalElement _) = RSESystemSignal
-- elementDestType (SystemSignalGroupElement _) = RSESystemSignalGroup
-- elementDestType (SystemTimingElement _) = RSESystemTiming
-- elementDestType (TdEventBswElement _) = RSETdEventBsw
-- elementDestType (TdEventBswInternalBehaviorElement _) = RSETdEventBswInternalBehavior
-- elementDestType (TdEventBswModeDeclarationElement _) = RSETdEventBswModeDeclaration
-- elementDestType (TdEventBswModuleElement _) = RSETdEventBswModule
-- elementDestType (TdEventComElement _) = RSETdEventCom
-- elementDestType (TdEventComplexElement _) = RSETdEventComplex
-- elementDestType (TdEventCycleStartElement _) = RSETdEventCycleStart
-- elementDestType (TdEventFrClusterCycleStartElement _) = RSETdEventFrClusterCycleStart
-- elementDestType (TdEventFrameElement _) = RSETdEventFrame
-- elementDestType (TdEventIPduElement _) = RSETdEventIPdu
-- elementDestType (TdEventISignalElement _) = RSETdEventISignal
-- elementDestType (TdEventModeDeclarationElement _) = RSETdEventModeDeclaration
-- elementDestType (TdEventOperationElement _) = RSETdEventOperation
-- elementDestType (TdEventSwcElement _) = RSETdEventSwc
-- elementDestType (TdEventSwcInternalBehaviorElement _) = RSETdEventSwcInternalBehavior
-- elementDestType (TdEventSwcInternalBehaviorReferenceElement _) = RSETdEventSwcInternalBehaviorReference
-- elementDestType (TdEventTtCanCycleStartElement _) = RSETdEventTtCanCycleStart
-- elementDestType (TdEventTriggerElement _) = RSETdEventTrigger
-- elementDestType (TdEventVariableDataPrototypeElement _) = RSETdEventVariableDataPrototype
-- elementDestType (TdEventVfbElement _) = RSETdEventVfb
-- elementDestType (TdEventVfbPortElement _) = RSETdEventVfbPort
-- elementDestType (TdEventVfbReferenceElement _) = RSETdEventVfbReference
-- elementDestType (TimeSyncServerConfigurationElement _) = RSETimeSyncServerConfiguration
-- elementDestType (TimingConstraintElement _) = RSETimingConstraint
-- elementDestType (TimingDescriptionElement _) = RSETimingDescription
-- elementDestType (TimingDescriptionEventElement _) = RSETimingDescriptionEvent
-- elementDestType (TimingDescriptionEventChainElement _) = RSETimingDescriptionEventChain
-- elementDestType (TimingEventElement _) = RSETimingEvent
-- elementDestType (TimingExtensionElement _) = RSETimingExtension
-- elementDestType (Topic1Element _) = RSETopic1
-- elementDestType (TpAddressElement _) = RSETpAddress
-- elementDestType (TpConfigElement _) = RSETpConfig
-- elementDestType (TraceReferrableElement _) = RSETraceReferrable
-- elementDestType (TraceableElement _) = RSETraceable
-- elementDestType (TraceableTextElement _) = RSETraceableText
-- elementDestType (TriggerElement _) = RSETrigger
elementDestType (TriggerInterfaceElement _) = RSETriggerInterface
-- elementDestType (TriggerInterfaceMappingElement _) = RSETriggerInterfaceMapping
-- elementDestType (TtcanClusterElement _) = RSETtcanCluster
-- elementDestType (TtcanCommunicationConnectorElement _) = RSETtcanCommunicationConnector
-- elementDestType (TtcanCommunicationControllerElement _) = RSETtcanCommunicationController
-- elementDestType (TtcanPhysicalChannelElement _) = RSETtcanPhysicalChannel
-- elementDestType (UdpNmClusterElement _) = RSEUdpNmCluster
-- elementDestType (UdpNmNodeElement _) = RSEUdpNmNode
-- elementDestType (UnitElement _) = RSEUnit
elementDestType (UnitGroupElement _) = RSEUnitGroup
-- elementDestType (UserDefinedClusterElement _) = RSEUserDefinedCluster
-- elementDestType (UserDefinedCommunicationConnectorElement _) = RSEUserDefinedCommunicationConnector
-- elementDestType (UserDefinedCommunicationControllerElement _) = RSEUserDefinedCommunicationController
-- elementDestType (UserDefinedIPduElement _) = RSEUserDefinedIPdu
-- elementDestType (UserDefinedPduElement _) = RSEUserDefinedPdu
-- elementDestType (UserDefinedPhysicalChannelElement _) = RSEUserDefinedPhysicalChannel
elementDestType (VariableAccessElement _) = RSEVariableAccess
-- elementDestType (VariableAndParameterInterfaceMappingElement _) = RSEVariableAndParameterInterfaceMapping
elementDestType (VariableDataPrototypeElement _) = RSEVariableDataPrototype
-- elementDestType (VariationPointProxyElement _) = RSEVariationPointProxy
-- elementDestType (VfbTimingElement _) = RSEVfbTiming
-- elementDestType (ViewMapElement _) = RSEViewMap
-- elementDestType (ViewMapSetElement _) = RSEViewMapSet
-- elementDestType (VlanConfigElement _) = RSEVlanConfig
-- elementDestType (WaitPointElement _) = RSEWaitPoint
-- elementDestType (WarningIndicatorRequestedBitNeedsElement _) = RSEWarningIndicatorRequestedBitNeeds
-- elementDestType (WorstCaseHeapUsageElement _) = RSEWorstCaseHeapUsage
-- elementDestType (WorstCaseStackUsageElement _) = RSEWorstCaseStackUsage
-- elementDestType (XcpPduElement _) = RSEXcpPdu
-- elementDestType (XdocElement _) = RSEXdoc
-- elementDestType (XfileElement _) = RSEXfile
-- elementDestType (XrefTargetElement _) = RSEXrefTarget

elementDestType x = error ("elementDestType not defined for " ++ (show x))
