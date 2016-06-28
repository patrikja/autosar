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

{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.MultiMap as MMap
import qualified Data.Char
import qualified Data.List as List
import           Data.Maybe
import qualified Text.XML as XML
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified System.Directory
import qualified System.Environment
import qualified System.Posix.Directory
import qualified System.FilePath
import           Text.PrettyPrint


main :: IO ()
main = do [f] <- System.Environment.getArgs
          convert f

swarch = convert "swarch.arxml"

convert filepath = do
    doc <- XML.readFile XML.def filepath
    let dir = System.FilePath.dropExtension filepath
    System.Directory.createDirectoryIfMissing True dir
    putStrLn ("Creating dir " ++ dir)
    System.Posix.Directory.changeWorkingDirectory dir
    let top = XML.documentRoot doc
    convertPackages top [] top
    System.Posix.Directory.changeWorkingDirectory ".."

type Tag = Text
type Name = Text
type Path = [Text] -- reverse list of path elements

convertPackages top path node =
    case optChild "AR-PACKAGES" node of
        Nothing ->
            return ()
        Just node' -> do
            mapM (convertPackage top path) $ children "AR-PACKAGE" node'
            return ()

convertPackage top path node = do
    info $ txt (pathStr path')
    writeModule path name $ convertPackageElements top path' node
    convertPackages top path' node
  where
    path' = name : path
    name = mkUp (shortName node)

convertPackageElements top path node =
    case optChild "ELEMENTS" node of
        Nothing ->
            empty
        Just node' ->
            txt "{-# LANGUAGE RecordWildCards #-}" $$
            txt "{-# LANGUAGE DeriveDataTypeable #-}" $$
            txt "{-# LANGUAGE TypeFamilies #-}" $$
            txt "{-# LANGUAGE UndecidableInstances #-}" $$
            txt "module" <+> txt modname <+> txt "where" <> nl $$
            convertImports top modname node' <> nl $$
            vmap (convertElement top path) (children "*" node') $$
            nl
  where modname = pathStr path

convertImports top modname node =
    txt "import NewARSim" $$
    vmap ((txt "import qualified" <+>) . txt) (List.nub (getRefs node) List.\\ [modname])
  where
    getRefs node
        | tag `elem` refTags0 =
            getRefs (lookupNode top (nodeVal node))
        | tag `elem` refTags1 = 
            [Text.intercalate "." (map mkUp (init names))]
        | tag `elem` refTags2 = 
            [Text.intercalate "." (map mkUp (init (init names)))]
        | tag `elem` refTags3 = 
            Text.intercalate "." (map mkUp (init names)) : getRefs (lookupNode top (nodeVal node))
        | otherwise =
            List.concatMap getRefs (children "*" node)
      where names = tail $ Text.splitOn "/" (nodeVal node)
            tag   = tagOf node

refTags0 = [ "CONSTANT-REF" ]

refTags1 = [ "TYPE-TREF", "SOFTWARE-COMPOSITION-TREF", "BASE-TYPE-REF" ]

refTags2 = [ "TARGET-P-PORT-REF", "TARGET-R-PORT-REF",
             "OPERATION-PROTOTYPE-REF", "DATA-ELEMENT-PROTOTYPE-REF" ]

refTags3 = ["REQUIRED-INTERFACE-TREF", "PROVIDED-INTERFACE-TREF" ]


convertElement top path node =
    case tagOf node of
--      "CONSTANT-SPECIFICATION"    -- inlined
        "SYSTEM" ->
            txt (mkLo (shortName root)) <+> txt "=" <+> convertQualName mkLo (leafVal "SOFTWARE-COMPOSITION-TREF" root)
          where
            root = grandChild "ROOT-SOFTWARE-COMPOSITIONS" "ROOT-SW-COMPOSITION-PROTOTYPE" node
        "COMPOSITION-SW-COMPONENT-TYPE" ->
            convertInterfaceType top name ports <> nl $$
            txt (mkLo name) <+> txt ":: AUTOSAR" <+> txt tname $$
            txt (mkLo name) <+> equals <+> txt "composition $ do" $$ 
            nest 4 (
                -- vmap (convertPort top) ports $$
                vmap (convertComp top) components $$
                vmap (convertAssemblyConn top) assemblies $$
                convertDelegationConns top tname delegates $$
                txt "return" <+> txt tname <+> txt "{..}" )
          where
            ports      = grandChildren "PORTS" "*" node
            components = grandChildren "COMPONENTS" "*" node
            assemblies = grandChildren "CONNECTORS" "ASSEMBLY-SW-CONNECTOR" node
            delegates  = grandChildren "CONNECTORS" "DELEGATION-SW-CONNECTOR" node
        "APPLICATION-SW-COMPONENT-TYPE" ->
            convertInterfaceType top name ports <> nl $$
            txt (mkLo name) <+> txt ":: AUTOSAR" <+> txt tname $$
            txt (mkLo name) <+> equals <+> txt "atomic $ do" $$ 
            nest 4 (
                vmap (convertPort top) ports $$
                vmap (convertInternalBehavior top name) behaviors $$
                txt "return $" <+> convertInterfaceTerm top name ports )
--          nl <> vmap (convertStubs (equals <+> txt "undefined") name) behaviors
          where
            ports      = grandChildren "PORTS" "*" node
            behaviors  = grandChildren "INTERNAL-BEHAVIORS" "SWC-INTERNAL-BEHAVIOR" node
        "SERVICE-SW-COMPONENT-TYPE" ->
            convertInterfaceType top name ports <> nl $$
            txt (mkLo name) <+> txt ":: AUTOSAR" <+> txt tname $$
            txt (mkLo name) <+> equals <+> txt "atomic $ do" $$ 
            nest 4 (
                vmap (convertPort top) ports $$
                vmap (convertInternalBehavior top name) behaviors $$
                txt "return $" <+> convertInterfaceTerm top name ports )
--          nl <> vmap (convertStubs (equals <+> txt "undefined") name) behaviors
          where
            ports      = grandChildren "PORTS" "*" node
            behaviors  = grandChildren "INTERNAL-BEHAVIORS" "SWC-INTERNAL-BEHAVIOR" node
        "APPLICATION-PRIMITIVE-DATA-TYPE" ->
            txt "type" <+> txt tname <+> equals <+> convertPrimitiveTypeCategory (leafVal "CATEGORY" node)
        "APPLICATION-RECORD-DATA-TYPE" ->
            txt "data" <+> txt tname <+> equals <+> txt tname  <+> txt "{" $$
            nest 4 (vcat . punctuate comma $ map (convertRecordElem top name) elems) <+> txt "}" <+>
            txt "deriving Data"
          where
            elems = grandChildren "ELEMENTS" "APPLICATION-RECORD-ELEMENT" node
        "SENDER-RECEIVER-INTERFACE" ->
            case grandChildren "DATA-ELEMENTS" "VARIABLE-DATA-PROTOTYPE" node of
                [delem] ->
                    nl <> txt "type" <+> txt tname <+> txt "r c" <+> equals <+> convertDataElem top delem $$
                    convertFieldname top tname (shortName delem) <+> equals <+> txt "id"
                delems ->
                    nl <> txt "data" <+> txt tname <+> txt "r c =" <+> txt tname <+> lbrace $$
                    nest 4 (vcat . punctuate comma $ map (convertDataElemField top tname) delems) $$
                    nest 2 rbrace $$
                    makePortInstanceSR top tname delems
        "CLIENT-SERVER-INTERFACE" ->
--          case grandChildren "OPERATIONS" "OPERATION-PROTOTYPE" node of       -- r3.x
            case grandChildren "OPERATIONS" "CLIENT-SERVER-OPERATION" node of   -- r4.0
                [op] ->
                    nl <> txt "type" <+> txt tname <+> txt "r c" <+> equals <+> convertOp top op $$
                    convertFieldname top tname (shortName op) <+> equals <+> txt "id"
                ops ->
                    nl <> txt "data" <+> txt tname <+> txt "r c =" <+> txt tname <+> lbrace $$
                    nest 4 (vcat . punctuate comma $ map (convertOpField top tname) ops) $$
                    nest 2 rbrace $$
                    makePortInstanceCS top tname ops
        "SW-BASE-TYPE" ->
            txt "type" <+> txt tname <+> equals <+> case leafVal "BASE-TYPE-ENCODING" node of
                                                        "ISO-8859-1" -> txt "String"
                                                        "ISO-8859-2" -> txt "String"
                                                        "WINDOWS-1252" -> txt "String"
                                                        "UTF-8" -> txt "Char"
                                                        "UTF-16" -> txt "Char"
                                                        "UCS-2" -> txt "Char"
                                                        "NONE" -> txt "Int"
                                                        "1C" -> txt "Int"
                                                        "2C" -> txt "Int"
                                                        "VOID" -> txt "()"
                                                        "BOOLEAN" -> txt "Bool"
                                                        _ -> txt "Double"
        "IMPLEMENTATION-DATA-TYPE" ->
            txt "type" <+> txt tname <+> equals <+>
            case defProps "BASE-TYPE-REF" node of
                [ref] -> convertQualName mkUp (nodeVal ref)
                _     -> convertPrimitiveTypeCategory (leafVal "CATEGORY" node)
        x ->
            txt "{-" <+> txt x <+> txt name <+> txt "-}"
  where
    name = shortName node
    tname = mkUp name

convertValueSpec top tref node = convertValueSpec1 top tref (grandChild "VALUE-SPEC" "*" node)

convertValueSpec1 top tref node =
    case tagOf node of
        "NUMERICAL-VALUE-SPECIFICATION"
            | Text.find (=='.') val == Nothing -> parens (txt "toEnum" <+> txt val)
            | otherwise                        -> txt val
        "RECORD-VALUE-SPECIFICATION" ->
            tcon <> braces (commasep (convertField top tname) (grandChildren "FIELDS" "*" node `zip` elems))
        tag ->
            txt "undefined"
    where tcon = convertQualName mkUp tref
          tname = last (Text.splitOn "/" tref)
          elems = grandChildren "ELEMENTS" "*" (lookupNode top tref)
          val   = leafVal "VALUE" node

convertField top tname (node, el) =
    convertFieldname top tname (leafVal "SHORT-LABEL" node) <+> equals <+> 
    convertValueSpec1 top (leafVal "TYPE-TREF" el) node

convertDataElemField top tname delem =
    convertFieldname top tname (shortName delem) <+> txt "::" <+> convertDataElem top delem

convertDataElem top delem =
    txt "DataElement" <+> convertQueued top delem <+> convertTRef top delem <+> txt "r c"

makePortInstanceSR top tname delems =
    txt "instance Port" <+> txt tname <+> txt "where" $$
    nest 4 (
        txt "providedPort" <+> equals <+> txt "do" $$
        nest 4 (
            vmap (\d -> fname d <+> txt "<-" <+> txt "providedPort") delems $$
            txt "return" <+> txt tname <> txt "{..}"
        ) $$
        txt "requiredPort" <+> equals <+> txt "do" $$
        nest 4 (
            vmap (\d -> fname d <+> txt "<-" <+> txt "requiredPort") delems $$
            txt "return" <+> txt tname <> txt "{..}"
        ) $$
        txt "connect a b = do" $$
        nest 4 (
            vmap (\d -> txt "connect" <+> parens (fname d <+> txt "a") <+> parens (fname d <+> txt "b")) delems
        ) $$
        txt "delegateP ps = do" $$
        nest 4 (
            vmap (\d -> fname d <+> txt "<-" <+> txt "providedDelegate" <+> 
                        brackets (txt " v |" <+> txt tname <> braces (fname d <+> equals <+> txt "v") <+> txt "<- ps ")) delems $$
            txt "return" <+> txt tname <> txt "{..}"
        ) $$
        txt "delegateR ps = do" $$
        nest 4 (
            vmap (\d -> fname d <+> txt "<-" <+> txt "requiredDelegate" <+> 
                        brackets (txt " v |" <+> txt tname <> braces (fname d <+> equals <+> txt "v") <+> txt "<- ps ")) delems $$
            txt "return" <+> txt tname <> txt "{..}"
        )
    )
  where
    fname d = convertFieldname top tname (shortName d)

convertOpField top tname op =
    convertFieldname top tname (shortName op) <+> txt "::" <+> convertOp top op
    
convertOp top op =
    txt "ClientServerOperation" <+> convertArgs top (grandChildren "ARGUMENTS" "ARGUMENT-PROTOTYPE" op) <+> txt "r c"

makePortInstanceCS top tname ios =
    undefined

convertPrimitiveTypeCategory "VALUE"            = txt "Double"
convertPrimitiveTypeCategory "BOOLEAN"          = txt "Bool"
convertPrimitiveTypeCategory "STRING"           = txt "String"
convertPrimitiveTypeCategory "TYPE_REFERENCE"   = txt "Int {- TYPE_REFERENCE -}"     -- Really dubious, of course...
convertPrimitiveTypeCategory "DATA_REFERENCE"   = txt "Int {- DATA_REFERENCE -}"     -- Really dubious, of course...
convertPrimitiveTypeCategory x                  = txt "?" <> txt x <> txt "?"


convertRecordElem top tname node =
    convertFieldname top tname (shortName node) <+> txt "::" <+> convertTRef top node

convertQueued1 top node
    | isQueued  = txt "Queued"
    | otherwise = txt "Unqueued"
  where 
    isQueued =
        case optChild "SW-DATA-DEF-PROPS" node of
            Just props ->
                case optChild "SW-DATA-DEF-PROPS-VARIANTS" props of
                    Just props ->
                        case optChild "SW-DATA-DEF-PROPS-CONDITIONAL" props of
                            Just props ->
                                case optChild "SW-IMPL-POLICY" props of
                                    Just policy ->
                                        nodeVal policy == "queued"
                                    _ -> False
                            _ -> False
                    _ -> False
            _ -> False

convertQueued top node = case defProps "SW-IMPL-POLICY" node of
                            [policy] -> if nodeVal policy == "queued" then txt "Queued" else txt "Unqueued"
                            _ -> txt "Unqueued"

defProps tag node = concat $ map (grandChildren "SW-DATA-DEF-PROPS-CONDITIONAL" tag)
                           $ grandChildren "SW-DATA-DEF-PROPS" "SW-DATA-DEF-PROPS-VARIANTS" node

convertArgs top args = parens (commasep (convertTRef top) ins) <+> parens (commasep (convertTRef top) outs)
  where
    ins  = filter ((`elem`["IN", "INOUT"]) . leafVal "DIRECTION") args
    outs = filter ((`elem`["OUT","INOUT"]) . leafVal "DIRECTION") args

convertTRef top node =
    convertQualName mkUp (leafVal "TYPE-TREF" node)

convertInterfaceType top name [] =
    nl <> txt "data" <+> txt (mkUp name) <+> equals <+> txt (mkUp name) <+> txt "()"
convertInterfaceType top name ports =
    nl <> txt "data" <+> txt (mkUp name) <+> equals <+> txt (mkUp name) <+> lbrace $$
    nest 4 (vcat . punctuate comma $ map (convertSignature top name) ports) $$
    nest 2 rbrace

convertInterfaceTerm top name [] =
    txt (mkUp name) <+> txt "()"
convertInterfaceTerm top name ports =
    txt "sealBy" <+> txt (mkUp name) <+> (hcat . punctuate space $ map (txt . mkLo . shortName) ports)

convertSignature top tname node =
    case tagOf node of
        "R-PORT-PROTOTYPE" ->
            fname <+> txt "::" <+> rTRef <+> txt "Required" <+> txt "Closed"
        "P-PORT-PROTOTYPE" ->
            fname <+> txt "::" <+> pTRef <+> txt "Provided" <+> txt "Closed"
        _ ->
            empty
    where
        fname = convertFieldname top tname (shortName node)
        rTRef = convertQualName mkUp (leafVal "REQUIRED-INTERFACE-TREF" node)
        pTRef = convertQualName mkUp (leafVal "PROVIDED-INTERFACE-TREF" node)

convertFieldname top tname name =
    txt $ mkLo $ Text.append tname $ Text.append "_" name

convertPort top node =
    case tagOf node of
        "R-PORT-PROTOTYPE" ->
            txt name <+> txt "<-" <+> txt "requiredPort" $$
            convertComSpecs top name (rComSpecs `zip` rElems)
        "P-PORT-PROTOTYPE" ->
            txt name <+> txt "<-" <+> txt "providedPort" $$
            convertComSpecs top name (pComSpecs `zip` pElems)
        _ ->
            empty
    where
        name = mkLo (shortName node)
        rComSpecs = grandChildren "REQUIRED-COM-SPECS" "*" node
        pComSpecs = grandChildren "PROVIDED-COM-SPECS" "*" node
        rElems = grandChildren "DATA-ELEMENTS" "*" $ lookupNode top $ leafVal "REQUIRED-INTERFACE-TREF" node
        pElems = grandChildren "DATA-ELEMENTS" "*" $ lookupNode top $ leafVal "PROVIDED-INTERFACE-TREF" node

convertComSpecs top name specs =
    vmap (convertComSpec top name) specs

convertComSpec top name (node,el) =
    case tagOf node of
        "NONQUEUED-RECEIVER-COM-SPEC" ->
            convInitVal
        "NONQUEUED-SENDER-COM-SPEC" ->
            convInitVal
        "QUEUED-RECEIVER-COM-SPEC" ->
            convQueueLength
        "QUEUED-SENDER-COM-SPEC" ->
            empty
        "SERVER-COM-SPEC" ->
            convQueueLength
        "CLIENT-COM-SPEC" ->
            empty
    where
        convInitVal     = conv "INIT-VALUE" "InitValue" (convertExp top (leafVal "TYPE-TREF" el) . onlyChild)
        convQueueLength = conv "QUEUE-LENGTH" "QueueLength" (txt . nodeVal)
        conv spec con f = case optChild spec node of
                            Nothing -> empty
                            Just node' -> txt "comSpec" <+> txt name <+> parens (txt con <+> f node')

lookupNode top ref = walk top (tail $ Text.splitOn "/" ref)
    where
        walk node []            = node
        walk node (name:names)  = case [ n | n <- grandChildren "*" "*" node, hasName n name ] of
                                    [node'] -> walk node' names
                                    _       -> error ("Can't find path " ++ show (name:names) ++ " in " ++ show node)

convertExp top tref node = 
    case tagOf node of
        "CONSTANT-REFERENCE" ->
            convertValueSpec top tref $ lookupNode top (leafVal "CONSTANT-REF" node)
--            convertQualName mkLo (leafVal "CONSTANT-REF" node)
        _ -> 
            error ("Exp node: " ++ show node)

convertComp top node =
    case tagOf node of
        "SW-COMPONENT-PROTOTYPE" ->
            txt name <+> txt "<-" <+> convertQualName mkLo (leafVal "TYPE-TREF" node)
        _ ->
            empty
    where
        name = mkLo (shortName node)

convertQualName f ref = txt $ Text.intercalate "." items
  where
    names = tail $ Text.splitOn "/" ref
    items = map mkUp modules ++ [f element]
    modules = init names
    element = last names

convertQualName2 f ref = txt $ Text.intercalate "." items
  where
    names = tail $ Text.splitOn "/" ref
    items = map mkUp modules ++ [f element]
    path = init names
    modules = init path
    element = Text.append (last path) (Text.append "_" (last names))

convertAssemblyConn top node =
    txt "connect" <+>
    parens (convertSel "TARGET-P-PORT-REF" "CONTEXT-COMPONENT-REF" pnode) <+>
    parens (convertSel "TARGET-R-PORT-REF" "CONTEXT-COMPONENT-REF" rnode)
  where
    pnode = child "PROVIDER-IREF" node
    rnode = child "REQUESTER-IREF" node

convertSel tag1 tag2 node =
    convertQualName2 mkLo (leafVal tag1 node) <+> txt (mkLo (lastPathVal tag2 node))

convertDelegationConns top tname nodes = vmap (convertDelegation top tname) (MMap.assocs connmap)
    where connmap = MMap.fromList (map (extractDelegationConn top) nodes)

convertDelegation top tname (Right outer, multiple) =
    convertFieldname top tname outer <+> txt "<- requiredDelegate" <+> brackets (commasep id multiple)
convertDelegation top tname (Left outer, multiple) =
    convertFieldname top tname outer <+> txt "<- providedDelegate" <+> brackets (commasep id multiple)

extractDelegationConn top node =
    case (optChild "R-PORT-IN-COMPOSITION-INSTANCE-REF" inner,
          optChild "P-PORT-IN-COMPOSITION-INSTANCE-REF" inner) of
        (Just rnode, _) ->
            (Right outer, convertSel "TARGET-R-PORT-REF" "CONTEXT-COMPONENT-REF" rnode)
        (_, Just pnode) ->
            (Left outer, convertSel "TARGET-P-PORT-REF" "CONTEXT-COMPONENT-REF" pnode)
  where
    inner = child "INNER-PORT-IREF" node
    outer = lastPathVal "OUTER-PORT-REF" node

convertInternalBehavior top comp node =
    vmap (convertRunnable top comp events vars excl) runnables
  where
    runnables = grandChildren "RUNNABLES" "RUNNABLE-ENTITY" node
    events = grandChildren "EVENTS" "*" node
    vars = grandChildren "EXPLICIT-INTER-RUNNABLE-VARIABLES" "VARIABLE-DATA-PROTOTYPE" node
    excl = grandChildren "EXCLUSIVE-AREAS" "EXCLUSIVE-AREA" node

convertRunnable top comp events vars excl node =
    runnable <+> arg <+> brackets (commasep (convertEvent top) myEvents) <+> txt "$ do" $$ nest 4 (txt "undefined")
  where
    name        = shortName node
    concurrent  = leafVal "CAN-BE-INVOKED-CONCURRENTLY" node
    minStart    = leafVal "MINIMUM-START-INTERVAL" node
    arg         = if concurrent == "true" then txt "Concurrent" else parens (txt "MinInterval" <+> txt minStart)
    myEvents    = filter (handledEvent name) events
    opInvoked   = filter (isTag "OPERATION-INVOKED") myEvents
    runnable    = if null opInvoked then txt "runnable" else txt "serverRunnable"

handledEvent rname event =
    mkLo (lastPathVal "START-ON-EVENT-REF" event) == rname &&
    tagOf event `elem` ["OPERATION-INVOKED","DATA-RECEIVED-EVENT","TIMING-EVENT","INIT-EVENT"]

convertEvent top node =
    case tagOf node of
        "OPERATION-INVOKED" ->
            txt "OperationInvokedEvent" <+> parens operRef
        "DATA-RECEIVED-EVENT" ->
            txt "DataReceivedEvent" <+> parens elemRef
        "TIMING-EVENT" ->
            txt "TimingEvent" <+> txt (leafVal "PERIOD" node)
        "INIT-EVENT" ->
            txt "InitEvent"
        _ ->
            empty
  where
    operRef = convertSel "OPERATION-PROTOTYPE-REF" "P-PORT-PROTOTYPE-REF" (child "OPERATION-IREF" node)
    elemRef = convertSel "DATA-ELEMENT-PROTOTYPE-REF" "R-PORT-PROTOTYPE-REF" (child "DATA-IREF" node)


{-
convertStubs comp rhs behavior =
    vmap (convertStub comp) (grandChildren "RUNNABLES" "RUNNABLE-ENTITY" behavior)

convertStub comp rhs runnable =
    txt (shortName runnable) <+> punctuate space args <+> rhs
  where
    "EXCLUSIVE-AREA-REF"
    "CAN-ENTER-EXCLUSIVE-AREA"
    "READ-LOCAL-VARIABLES"
    "WRITTEN-LOCAL-VARIABLES"
    "DATA-RECEIVE-POINT-BY-VALUE"
    "DATA-RECEIVE-POINT-BY-ARGUMENT"
    "SERVER-CALL-POINT"
    "DATA-SEND-POINT"
    "ASYNCHRONOUS-SERVER-CALL-RESULT-POINT"
-}



mkLo s = if Data.Char.isLower (Text.head s) then s else Text.cons 'x' s

mkUp s = if Data.Char.isUpper (Text.head s) then s else Text.cons 'X' s

splitPath :: Text -> [Text]
splitPath = Text.splitOn "/"

lastPathVal tag = last . splitPath . leafVal tag

pathStr path = Text.intercalate "." (reverse path)

leafVal tag node = nodeVal $ child tag node

nodeVal node = case XML.elementNodes node of
                  [XML.NodeContent str] -> str
                  _ -> error ("#### Bad nodeVal: " ++ show node)

shortName = leafVal "SHORT-NAME"

tagOf node =
    XML.nameLocalName (XML.elementName node)

isTag tag node =
    tag == tagOf node

child tag node =
    case optChild tag node of
        Just node' -> node'
        _ -> error ("#Bad tag: " ++ Text.unpack tag ++ " (has: " ++ 
                    render (commasep (txt . tagOf) (children "*" node)) ++ ")")

hasName node name =
    case optChild "SHORT-NAME" node of
        Just n -> name == nodeVal n
        _      -> False

hasChild tag node =
    case optChild tag node of
        Just _ -> True
        _ -> False

optChild tag =
    listToMaybe . children tag

onlyChild node = head (children "*" node)

children "*" node =
    [ e | XML.NodeElement e <- XML.elementNodes node ]
children tag node =
    filter (isTag tag) $ children "*" node

grandChildren tag1 tag2 =
    List.concatMap (children tag2) . children tag1

grandChild tag1 tag2 =
    head . grandChildren tag1 tag2

txt = text . Text.unpack

table f = vmap (txt . f)

vmap f = vcat . map f

commasep f = hcat . punctuate comma . map f

nl = text "\n"

info = putStrLn . render

writeModule revPath name doc
    | isEmpty doc =
        return ()
    | otherwise = do
        System.Directory.createDirectoryIfMissing True dirPath
        writeFile filePath (render doc)
    where
        path = map Text.unpack (reverse revPath)
        dirPath = System.FilePath.joinPath path
        filePath = dirPath System.FilePath.</> Text.unpack name System.FilePath.<.> "hs"

