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

import qualified Data.Map
import qualified Data.Char
import qualified Data.List as List
import Data.Maybe
import qualified Text.XML as XML
import qualified Data.Text as Text
import qualified System.Directory
import qualified System.FilePath
import Text.PrettyPrint


convertXML f = do
    doc <- XML.readFile XML.def f
    optPackages [] $ XML.documentRoot doc

optPackages path node = 
    case optChild "AR-PACKAGES" node of
        Nothing -> 
            return ()
        Just node' -> do
            mapM (convertPackage path) $ children "AR-PACKAGE" node'
            return ()

convertPackage path node = do
    out $ nl <> txt (pathStr path') <> nl
    write path name $ optElements path' node
    optPackages path' node
  where 
    path' = name : path
    name = mkUp (shortName node)


optElements path node =
    case optChild "ELEMENTS" node of
        Nothing ->
            empty
        Just node' ->
            txt "module" <+> txt (pathStr path) <+> txt "where" $$
            vmap (convertElement path) (allChildren node')

convertElement path node =
    convert tag name node
  where
    name = leafVal "SHORT-NAME" node
    tag = tagOf node

convert "COMPOSITION-SW-COMPONENT-TYPE" name node =
    nl <> txt "data" <+> txt (mkUp name) <+> equals <+> lbrace $$
    nest 4 (vmap convertSignature ports) $$
    nest 2 rbrace $$
    nl <> txt (mkLo name) <+> equals <+> txt "do" $$
    vmap convertPort ports $$
    vmap convertComp components $$
    vmap convertConn connectors
  where
    ports = grandChildren "PORTS" node
    components = grandChildren "COMPONENTS" node
    connectors = grandChildren "CONNECTORS" node
convert _ name node =
    empty

convertSignature node
    | isTag "R-PORT-PROTOTYPE" node =
        txt name <+> txt "::" <+> txt "RequiredDataElem" <+> txt rTRef
    | isTag "P-PORT-PROTOTYPE" node =
        txt name <+> txt "::" <+> txt "ProvidedDataElem" <+> txt pTRef
    | otherwise =
        empty
    where
        name = mkLo (shortName node)
        rTRef = mkQual mkUp "REQUIRED-INTERFACE-TREF" node
        pTRef = mkQual mkUp "PROVIDED-INTERFACE-TREF" node

convertPort node
    | isTag "R-PORT-PROTOTYPE" node =
        nest 4 $ txt name <+> txt "<-" <+> txt "requiredDataElement"
    | isTag "P-PORT-PROTOTYPE" node =
        nest 4 $ txt name <+> txt "<-" <+> txt "providedDataElement"
    | otherwise =
        empty
    where
        name = mkLo (shortName node)
        rComSpec = child "REQUIRED-COM-SPECS" node
        pComSpec = child "PROVIDED-COM-SPECS" node
    
convertComp node
    | isTag "SW-COMPONENT-PROTOTYPE" node =
        nest 4 $ txt name <+> txt "<-" <+> txt ref
    | otherwise =
        empty
    where
        name = mkLo (shortName node)
        ref = mkQual mkLo "TYPE-TREF" node

mkQual f tag node = Text.intercalate "." names'
  where
    names = tail $ Text.splitOn "/" $ leafVal tag node
    names' = map mkUp (init names) ++ [f (last names)]

convertConn node
    | isTag "ASSEMBLY-SW-CONNECTOR" node =
        convertAssemblyConn node
    | isTag "DELEGATION-SW-CONNECTOR" node =
        convertDelegationConn node
    | otherwise =
        empty

convertAssemblyConn node =
    nest 4 $ txt "connect" <+>
    txt (lastPathVal "CONTEXT-COMPONENT-REF" pnode) <> txt "." <>
    txt (lastPathVal "TARGET-P-PORT-REF" pnode) <+>
    txt (lastPathVal "CONTEXT-COMPONENT-REF" rnode) <> txt "." <>
    txt (lastPathVal "TARGET-R-PORT-REF" rnode)
  where
    pnode = child "PROVIDER-IREF" node
    rnode = child "REQUESTER-IREF" node

convertDelegationConn node =
    case (optChild "R-PORT-IN-COMPOSITION-INSTANCE-REF" inner,
          optChild "P-PORT-IN-COMPOSITION-INSTANCE-REF" inner) of
        (Just rnode, _) ->
            nest 4 $ txt "connect" <+> txt outp <+> 
            txt (lastPathVal "CONTEXT-COMPONENT-REF" rnode) <> txt "." <>
            txt (lastPathVal "TARGET-R-PORT-REF" rnode)
        (_, Just pnode) ->
            nest 4 $ txt "connect" <+> 
            txt (lastPathVal "CONTEXT-COMPONENT-REF" pnode) <> txt "." <>
            txt (lastPathVal "TARGET-P-PORT-REF" pnode) <+> txt outp
    where
        inner = child "INNER-PORT-IREF" node
        outp = lastPathVal "OUTER-PORT-REF" node

mkLo s = if Data.Char.isLower (Text.head s) then s else Text.cons 'x' s

mkUp s = if Data.Char.isUpper (Text.head s) then s else Text.cons 'X' s

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
        _ -> error ("#Bad tag: " ++ Text.unpack tag)

hasChild tag node =
    case optChild tag node of
        Just _ -> True
        _ -> False

optChild tag =
    listToMaybe . children tag
    
children tag =
    filter (isTag tag) . allChildren 

grandChildren tag =
    List.concatMap allChildren . children tag
    
allChildren node =
    [ e | XML.NodeElement e <- XML.elementNodes node ]

txt = text . Text.unpack

table f = vmap (txt . f)

vmap f = vcat . map f

nl = text "\n"

out = putStr . render

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
