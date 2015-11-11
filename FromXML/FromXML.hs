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
          convertXML f

convertXML :: FilePath -> IO ()
convertXML f = do
    doc <- XML.readFile XML.def f
    let dir = System.FilePath.dropExtension f
    System.Directory.createDirectoryIfMissing True dir
    putStr ("Creating dir " ++ dir)
    System.Posix.Directory.changeWorkingDirectory dir
    optPackages [] $ XML.documentRoot doc
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
    out $ nl <> txt (pathStr path') <> nl
    write path name $ optElements path' node
    optPackages path' node
  where
    path' = name : path
    name = mkUp (shortName node)

optElements :: Path -> XML.Element -> Doc
optElements path node =
    case optChild "ELEMENTS" node of
        Nothing ->
            empty
        Just node' ->
            txt "{-# LANGUAGE RecordWildCards #-}" $$
            txt "module" <+> txt (pathStr path) <+> txt "where" $$
            vmap (convertElement path) (children "*" node') $$
            txt "\n"

convertElement :: Path -> XML.Element -> Doc
convertElement path node =
    convert tag name node
  where
    name = leafVal "SHORT-NAME" node
    tag = tagOf node

type Tag = Text
type Name = Text
convert :: Tag -> Name -> XML.Element -> Doc
convert "COMPOSITION-SW-COMPONENT-TYPE" name node =
    convertInterfaceType name ports $$
    nl <> txt (mkLo name) <+> equals <+> txt "do" $$ 
    nest 4 (
    vmap convertComp components $$
    vmap convertAssemblyConn assemblies $$
    convertDelegationConns delegates $$
    txt "return" <+> txt (mkUp name) <+> txt "{..}" )
  where
    ports      = grandChildren "*" "PORTS" node
    components = grandChildren "*" "COMPONENTS" node
    assemblies = grandChildren "ASSEMBLY-SW-CONNECTOR" "CONNECTORS" node
    delegates  = grandChildren "DELEGATION-SW-CONNECTOR" "CONNECTORS" node
convert "APPLICATION-SW-COMPONENT-TYPE" name node =
    convertInterfaceType name ports $$
    nl <> txt (mkLo name) <+> equals <+> txt "do" $$ 
    nest 4 (
    vmap convertPort ports $$
    txt "return" <+> txt (mkUp name) <+> txt "{..}" )
  where
    ports      = grandChildren "*" "PORTS" node
convert "SENDER-RECEIVER-INTERFACE" name node =
    case grandChildren "*" "DATA-ELEMENTS" node of
        [delem] ->
            nl <> txt "type" <+> txt (mkUp name) <+> txt "r c = DataElement Unqueued" <+> 
            txt (mkQual mkUp "TYPE-TREF" delem) <+> txt "r c"
convert _ name node =
    empty

convertInterfaceType name ports =
    nl <> txt "data" <+> txt (mkUp name) <+> txt "c" <+> equals <+> lbrace $$
    nest 4 (vmap convertSignature ports) $$
    nest 2 rbrace

convertInterfaceTerm name ports = empty

convertSignature :: XML.Element -> Doc
convertSignature node
    | isTag "R-PORT-PROTOTYPE" node =
        txt name <+> txt "::" <+> txt rTRef <+> txt "Required" <+> txt "c"
    | isTag "P-PORT-PROTOTYPE" node =
        txt name <+> txt "::" <+> txt pTRef <+> txt "Provided" <+> txt "c"
    | otherwise =
        empty
    where
        name = mkLo (shortName node)
        rTRef = mkQual mkUp "REQUIRED-INTERFACE-TREF" node
        pTRef = mkQual mkUp "PROVIDED-INTERFACE-TREF" node

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
        txt "UnqueuedSenderComSpec{ initSend =" <+> initVal <+> txt "}"
    | isTag "NONQUEUED-SENDER-COM-SPEC" node =
        txt "UnqueuedReceiverComSpec{ initValue =" <+> initVal <+> txt "}"
    | otherwise =
        error ("ComSpec node: " ++ show node)
    where
        initVal = case optChild "INIT-VALUE" node of 
                    Nothing -> txt "Nothing"
                    Just node' -> txt "Just" <+> convertExp (onlyChild node')

convertExp :: XML.Element -> Doc
convertExp node
    | isTag "CONSTANT-REFERENCE" node =
        txt $ mkQual mkLo "CONSTANT-REF" node
    | otherwise =
        error ("Exp node: " ++ show node)

convertComp :: XML.Element -> Doc
convertComp node
    | isTag "SW-COMPONENT-PROTOTYPE" node =
        txt name <+> txt "<-" <+> txt ref
    | otherwise =
        empty
    where
        name = mkLo (shortName node)
        ref = mkQual mkLo "TYPE-TREF" node

mkQual :: (Text -> Text) -> Text -> XML.Element -> Text
mkQual f tag node = Text.intercalate "." names'
  where
    names = tail $ Text.splitOn "/" $ leafVal tag node
    names' = map mkUp (init names) ++ [f (last names)]

convertAssemblyConn :: XML.Element -> Doc
convertAssemblyConn node =
    txt "connect" <+>
    parens (convertSel "TARGET-P-PORT-REF" "CONTEXT-COMPONENT-REF" pnode) <+>
    parens (convertSel "TARGET-R-PORT-REF" "CONTEXT-COMPONENT-REF" rnode)
  where
    pnode = child "PROVIDER-IREF" node
    rnode = child "REQUESTER-IREF" node

convertSel tag1 tag2 node =
    txt (mkQual mkLo tag1 node) <+> txt (lastPathVal tag2 node)

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

mkLo :: Text -> Text
mkLo s = if Data.Char.isLower (Text.head s) then s else Text.cons 'x' s

mkUp :: Text -> Text
mkUp s = if Data.Char.isUpper (Text.head s) then s else Text.cons 'X' s

splitPath :: Text -> [Text]
splitPath = Text.splitOn "/"

lastPathVal tag = mkLo . last . splitPath . leafVal tag

pathStr path = Text.intercalate "." (reverse path)

leafVal tag node = str
  where [XML.NodeContent str] = XML.elementNodes $ child tag node

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
    List.concatMap (children tag1) . children tag2


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
