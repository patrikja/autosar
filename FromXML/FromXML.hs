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
module FromXML where

import qualified Data.MultiMap as MMap
import qualified Data.Char
import qualified Data.List as List
import Data.Maybe
import qualified Text.XML as XML
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified System.Directory
import qualified System.Environment
import qualified System.Posix.Directory
import qualified System.FilePath
import Text.PrettyPrint


main :: IO ()
main = do [f] <- System.Environment.getArgs
          convert f

convert :: FilePath -> IO ()
convert f = do
    doc <- XML.readFile XML.def f
    let dir = System.FilePath.dropExtension f
    System.Directory.createDirectoryIfMissing True dir
    putStr ("Creating dir " ++ dir)
    System.Posix.Directory.changeWorkingDirectory dir
    optPackages [] $ XML.documentRoot doc
    putStr "\n"
    System.Posix.Directory.changeWorkingDirectory ".."

type Path = [Text] -- reverse list of path elements
optPackages :: Path -> XML.Element -> IO ()
optPackages path node =
    case optChild "AR-PACKAGES" node of
        Nothing ->
            return ()
        Just node' -> do
            mapM (convertPackage path) $ children "AR-PACKAGE" node'
            return ()

convertPackage :: Path -> XML.Element -> IO ()
convertPackage path node = do
    out $ nl <> txt (pathStr path')
    write path name $ optPackageElements path' node
    optPackages path' node
  where
    path' = name : path
    name = mkUp (shortName node)

optPackageElements :: Path -> XML.Element -> Doc
optPackageElements path node =
    case optChild "ELEMENTS" node of
        Nothing ->
            empty
        Just node' ->
            txt "{-# LANGUAGE RecordWildCards #-}" $$
            txt "module" <+> txt modname <+> txt "where" <> nl $$
            convertImports modname node' $$
            vmap (convertElement path) (children "*" node') $$
            nl
  where modname = pathStr path

convertImports modname node =
    txt "import NewARSim" $$
    vmap ((txt "import qualified" <+>) . txt) (List.nub (getRefs node) List.\\ [modname])
  where
    getRefs node
        | tagOf node `elem` refTags1 = 
            [Text.intercalate "." (map mkUp (init names))]
        | tagOf node `elem` refTags2 = 
            [Text.intercalate "." (map mkUp (init (init names)))]
        | otherwise =
            List.concatMap getRefs (children "*" node)
      where names = tail $ Text.splitOn "/" (nodeVal node)

refTags1 = [ "TYPE-TREF", "REQUIRED-INTERFACE-TREF", "PROVIDED-INTERFACE-TREF", "CONSTANT-REF" ]

refTags2 = [ "TARGET-P-PORT-REF", "TARGET-R-PORT-REF",
             "OPERATION-PROTOTYPE-REF", "DATA-ELEMENT-PROTOTYPE-REF" ]

convertElement :: Path -> XML.Element -> Doc
convertElement path node =
    convertElem tag name node
  where
    name = shortName node
    tag = tagOf node

type Tag = Text
type Name = Text

convertElem :: Tag -> Name -> XML.Element -> Doc
convertElem "COMPOSITION-SW-COMPONENT-TYPE" name node =
    convertInterfaceType name ports $$
    nl <> txt (mkLo name) <+> equals <+> txt "composition $ do" $$ 
    nest 4 (
    vmap convertComp components $$
    vmap convertAssemblyConn assemblies $$
    convertDelegationConns delegates $$
    txt "return" <+> txt (mkUp name) <+> txt "{..}" )
  where
    ports      = grandChildren "PORTS" "*" node
    components = grandChildren "COMPONENTS" "*" node
    assemblies = grandChildren "CONNECTORS" "ASSEMBLY-SW-CONNECTOR" node
    delegates  = grandChildren "CONNECTORS" "DELEGATION-SW-CONNECTOR" node
convertElem "APPLICATION-SW-COMPONENT-TYPE" name node =
    convertInterfaceType name ports $$
    nl <> txt (mkLo name) <+> equals <+> txt "atomic $ do" $$ 
    nest 4 (
    vmap convertPort ports $$
    vmap (convertInternalBehavior name) behaviors $$
    txt "return" <+> txt (mkUp name) <+> txt "{..}" )
--    nl <> vmap (convertStubs (equals <+> txt "undefined") name) behaviors
  where
    ports      = grandChildren "PORTS" "*" node
    behaviors  = grandChildren "INTERNAL-BEHAVIORS" "SWC-INTERNAL-BEHAVIOR" node
convertElem "APPLICATION-PRIMITIVE-DATA-TYPE" name node =
    txt "type" <+> txt (mkUp name) <+> equals <+> convertPrimitiveTypeCategory (leafVal "CATEGORY" node)
convertElem "APPLICATION-RECORD-DATA-TYPE" name node =
    txt "data" <+> txt (mkUp name) <+> equals <+> txt (mkUp name)  <+> txt "{" $$
    nest 4 (vcat . punctuate comma $ map (convertRecordElem name) elems) <+> txt "}"
  where
    elems = grandChildren "ELEMENTS" "APPLICATION-RECORD-ELEMENT" node
convertElem "SENDER-RECEIVER-INTERFACE" name node =
    case grandChildren "DATA-ELEMENTS" "VARIABLE-DATA-PROTOTYPE" node of
        [delem] ->
            nl <> txt "type" <+> txt (mkUp name) <+> txt "r c = DataElement" <+> 
            convertQueued delem <+> convertTRef delem <+> txt "r c" $$
            convertFieldname name (shortName delem) <+> equals <+> txt "id"
        delems ->
            -- data ...
            error "not yet: multiple delems"
convertElem "CLIENT-SERVER-INTERFACE" name node =
    case grandChildren "OPERATIONS" "OPERATION-PROTOTYPE" node of
        [op] ->
            nl <> txt "type" <+> txt (mkUp name) <+> txt "r c = ClientServerOperation" <+>
            convertArgs (grandChildren "ARGUMENTS" "ARGUMENT-PROTOTYPE" op) <+> txt "r c" $$
            txt (mkLo (shortName op)) <+> equals <+> txt "id"
convertElem _ name node =
    empty

convertPrimitiveTypeCategory "VALUE"    = txt "Int"
convertPrimitiveTypeCategory "BOOLEAN"  = txt "Bool"
convertPrimitiveTypeCategory "STRING"   = txt "String"

convertRecordElem tname node =
    convertFieldname tname (shortName node) <+> txt "::" <+> convertTRef node

convertQueued node
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

convertArgs args = parens (commasep convertTRef ins) <+> parens (commasep convertTRef outs)
  where
    ins  = filter ((`elem`["IN", "INOUT"]) . leafVal "DIRECTION") args
    outs = filter ((`elem`["OUT","INOUT"]) . leafVal "DIRECTION") args

convertTRef node =
    convertQualName mkUp "TYPE-TREF" node

convertInterfaceType name ports =
    nl <> txt "data" <+> txt (mkUp name) <+> txt "c" <+> equals <+> lbrace $$
    nest 4 (vmap (convertSignature name) ports) $$
    nest 2 rbrace

convertInterfaceTerm name ports = empty

convertSignature :: Name -> XML.Element -> Doc
convertSignature tname node
    | isTag "R-PORT-PROTOTYPE" node =
        fname <+> txt "::" <+> rTRef <+> txt "Required" <+> txt "c"
    | isTag "P-PORT-PROTOTYPE" node =
        fname <+> txt "::" <+> pTRef <+> txt "Provided" <+> txt "c"
    | otherwise =
        empty
    where
        fname = convertFieldname tname (shortName node)
        rTRef = convertQualName mkUp "REQUIRED-INTERFACE-TREF" node
        pTRef = convertQualName mkUp "PROVIDED-INTERFACE-TREF" node

convertFieldname tname name =
    txt $ mkLo $ Text.append tname $ Text.append "_" name

convertPort :: XML.Element -> Doc
convertPort node
    | isTag "R-PORT-PROTOTYPE" node =
        txt name <+> txt "<-" <+> txt "require" <+> 
        (parens $ commasep convertComSpec $ children "*" rComSpec)
    | isTag "P-PORT-PROTOTYPE" node =
        txt name <+> txt "<-" <+> txt "provide" <+>
        (parens $ commasep convertComSpec $ children "*" pComSpec)
    | otherwise =
        empty
    where
        name = mkLo (shortName node)
        rComSpec = child "REQUIRED-COM-SPECS" node
        pComSpec = child "PROVIDED-COM-SPECS" node

convertComSpec :: XML.Element -> Doc
convertComSpec node
    | isTag "NONQUEUED-RECEIVER-COM-SPEC" node =
        txt "UnqueuedReceivererComSpec{ initSend =" <+> initVal <+> txt "}"
    | isTag "NONQUEUED-SENDER-COM-SPEC" node =
        txt "UnqueuedSenderComSpec{ initValue =" <+> initVal <+> txt "}"
    | isTag "QUEUED-RECEIVER-COM-SPEC" node =
        txt "QueuedReceiverComSpec{ queueLength =" <+> queueLength <+> txt "}"
    | isTag "QUEUED-SENDER-COM-SPEC" node =
        txt "QueuedSenderComSpec"
    | isTag "SERVER-COM-SPEC" node =
        txt "ServerComSpec{ bufferLength =" <+> queueLength <+> txt "}"
    | isTag "CLIENT-COM-SPEC" node =
        txt "ClientComSpec"
    where
        initVal = case optChild "INIT-VALUE" node of 
                    Nothing -> txt "Nothing"
                    Just node' -> txt "Just" <+> convertExp (onlyChild node')
        queueLength = txt (leafVal "QUEUE-LENGTH" node)

convertExp :: XML.Element -> Doc
convertExp node
    | isTag "CONSTANT-REFERENCE" node =
        convertQualName mkLo "CONSTANT-REF" node
    | otherwise =
        error ("Exp node: " ++ show node)

convertComp :: XML.Element -> Doc
convertComp node
    | isTag "SW-COMPONENT-PROTOTYPE" node =
        txt name <+> txt "<-" <+> convertQualName mkLo "TYPE-TREF" node
    | otherwise =
        empty
    where
        name = mkLo (shortName node)

convertQualName :: (Text -> Text) -> Text -> XML.Element -> Doc
convertQualName f tag node = txt $ Text.intercalate "." items
  where
    names = tail $ Text.splitOn "/" $ leafVal tag node
    items = map mkUp modules ++ [f element]
    modules = init names
    element = last names

convertQualName2 :: (Text -> Text) -> Text -> XML.Element -> Doc
convertQualName2 f tag node = txt $ Text.intercalate "." items
  where
    names = tail $ Text.splitOn "/" $ leafVal tag node
    items = map mkUp modules ++ [f element]
    path = init names
    modules = init path
    element = Text.append (last path) (Text.append "_" (last names))

convertAssemblyConn :: XML.Element -> Doc
convertAssemblyConn node =
    txt "connect" <+>
    parens (convertSel "TARGET-P-PORT-REF" "CONTEXT-COMPONENT-REF" pnode) <+>
    parens (convertSel "TARGET-R-PORT-REF" "CONTEXT-COMPONENT-REF" rnode)
  where
    pnode = child "PROVIDER-IREF" node
    rnode = child "REQUESTER-IREF" node

convertSel tag1 tag2 node =
    convertQualName2 mkLo tag1 node <+> txt (lastPathVal tag2 node)

convertDelegationConns :: [XML.Element] -> Doc
convertDelegationConns nodes = vmap convertDelegation (MMap.assocs connmap)
    where connmap = MMap.fromList (map extractDelegationConn nodes)

convertDelegation :: (Text,[Doc]) -> Doc
convertDelegation (outer, [single]) = 
    txt "let" <+> txt outer <+> equals <+> single
convertDelegation (outer, multiple) =
    txt outer <+> txt "<- delegate" <+> brackets (commasep id multiple)

extractDelegationConn :: XML.Element -> (Text,Doc)
extractDelegationConn node =
    case (optChild "R-PORT-IN-COMPOSITION-INSTANCE-REF" inner,
          optChild "P-PORT-IN-COMPOSITION-INSTANCE-REF" inner) of
        (Just rnode, _) ->
            (outer, convertSel "TARGET-R-PORT-REF" "CONTEXT-COMPONENT-REF" rnode)
        (_, Just pnode) ->
            (outer, convertSel "TARGET-P-PORT-REF" "CONTEXT-COMPONENT-REF" pnode)
  where
    inner = child "INNER-PORT-IREF" node
    outer = lastPathVal "OUTER-PORT-REF" node

convertInternalBehavior comp node =
    vmap (convertRunnable comp events vars excl) runnables
  where
    runnables = grandChildren "RUNNABLES" "RUNNABLE-ENTITY" node
    events = grandChildren "EVENTS" "*" node
    vars = grandChildren "EXPLICIT-INTER-RUNNABLE-VARIABLES" "VARIABLE-DATA-PROTOTYPE" node
    excl = grandChildren "EXCLUSIVE-AREAS" "EXCLUSIVE-AREA" node

convertRunnable comp events vars excl node =
    runnable <+> arg <+> brackets (commasep convertEvent myEvents) <+> txt "$ do" $$ nest 4 (txt "undefined")
  where
    name        = shortName node
    concurrent  = leafVal "CAN-BE-INVOKED-CONCURRENTLY" node
    minStart    = leafVal "MINIMUM-START-INTERVAL" node
    arg         = if concurrent == "true" then txt "Concurrent" else txt "MinInterval" <+> txt minStart
    myEvents    = filter (handledEvent name) events
    opInvoked   = filter (isTag "OPERATION-INVOKED") myEvents
    runnable    = if null opInvoked then txt "runnable" else txt "serverRunnable"

handledEvent rname event =
    lastPathVal "START-ON-EVENT-REF" event == rname &&
    tagOf event `elem` ["OPERATION-INVOKED","DATA-RECEIVED-EVENT","TIMING-EVENT","INIT-EVENT"]

convertEvent node
    | isTag "OPERATION-INVOKED" node =
        txt "OperationInvokedEvent" <+> parens operRef
    | isTag "DATA-RECEIVED-EVENT" node =
        txt "DataReceivedEvent" <+> parens elemRef
    | isTag "TIMING-EVENT" node =
        txt "TimingEvent" <+> txt (leafVal "PERIOD" node)
    | isTag "INIT-EVENT" node =
        txt "InitEvent"
    | otherwise =
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



mkLo :: Text -> Text
mkLo s = if Data.Char.isLower (Text.head s) then s else Text.cons 'x' s

mkUp :: Text -> Text
mkUp s = if Data.Char.isUpper (Text.head s) then s else Text.cons 'X' s

splitPath :: Text -> [Text]
splitPath = Text.splitOn "/"

lastPathVal tag = mkLo . last . splitPath . leafVal tag

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


txt = text . Text.unpack

table f = vmap (txt . f)

vmap f = vcat . map f

commasep f = hcat . punctuate comma . map f

nl = text "\n"

out = putStr . render

write :: Path -> Name -> Doc -> IO ()
write revPath name doc
    | isEmpty doc =
        return ()
    | otherwise = do
        System.Directory.createDirectoryIfMissing True dirPath
        writeFile filePath (render doc)
    where
        path = map Text.unpack (reverse revPath)
        dirPath = System.FilePath.joinPath path
        filePath = dirPath System.FilePath.</> Text.unpack name System.FilePath.<.> "hs"

