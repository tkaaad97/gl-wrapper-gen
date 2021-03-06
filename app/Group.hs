{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Group
    ( genGroupDeclaresCode
    , parseGroup
    , parseGroupMemberTypes
    , writeAll
    , writeGroupDeclaresCode
    ) where

import Control.Lens (filtered)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (findWithDefault, fromList)
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (member)
import qualified Data.Text as T (Text, concat, intercalate, splitOn, toLower,
                                 toTitle, unpack)
import qualified Data.Text.Lazy as LT (Text, intercalate)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Text.Shakespeare.Text (lt)
import qualified Text.XML as XML (Document, Element)
import Text.XML.Lens
import qualified Types

parseGroup :: Set T.Text -> Map T.Text Types.PrimType -> XML.Element -> Maybe Types.Group
parseGroup enumNames types elem' = do
    gname <- elem' ^? attr "name"
    let type' = Map.findWithDefault Types.GLenum gname types
        members = elem' ^.. el "group" ./ el "enum" . attr "name" . filtered (`Set.member` enumNames)
    return $ Types.Group gname members type'

parseGroupMemberTypes :: XML.Document -> Map T.Text Types.PrimType
parseGroupMemberTypes doc = Map.fromList . mapMaybe parseGroupMemberType $ protos ++ params
    where
    protos = doc ^.. root . el "registry" ./ el "commands" ./ el "command" ./ el "proto" . filtered hasGroupAttr
    params = doc ^.. root . el "registry" ./ el "commands" ./ el "command" ./ el "param" . filtered hasGroupAttr
    hasGroupAttr = isJust . (^? entire . attr "group")

parseGroupMemberType :: XML.Element -> Maybe (T.Text, Types.PrimType)
parseGroupMemberType elem' = do
    group <- elem' ^? entire . attr "group"
    ptype <- Types.parsePrimType . Just =<< (elem' ^? entire . el "ptype" . text)
    return (group, ptype)

writeAll :: FilePath -> [Types.Group] -> IO ()
writeAll outputPath groups = do
    writeGroupDeclaresCode outputPath groups
    writeGroupMemberDeclaresCodes outputPath groups

writeGroupDeclaresCode :: FilePath -> [Types.Group] -> IO ()
writeGroupDeclaresCode outputPath groups = do
    let code = genGroupDeclaresCode groups
        path = outputPath ++ "/GLW/Internal/Groups.hs"
    LT.writeFile path code

genGroupDeclaresCode :: [Types.Group] -> LT.Text
genGroupDeclaresCode groups =
    let groupNames = map Types.groupName groups
        groupDeclares = map genGroupDeclare groups
    in [lt|{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GLW.Internal.Groups
    ( #{T.intercalate "(..)\n    , " groupNames}(..)
    ) where

import Data.Bits (Bits(..))
import qualified Graphics.GL as GL

#{LT.intercalate "\n" groupDeclares}|]

genGroupDeclare :: Types.Group -> LT.Text
genGroupDeclare group | null (Types.groupMembers group) = ""
genGroupDeclare group =
    [lt|newtype #{groupName} = #{groupName} {
    un#{groupName} :: #{Types.printPrimType "GL." groupMemberType}
    } deriving (#{derivingClasses})
|]
    where
    groupName = Types.groupName group
    groupMemberType = Types.groupMemberType group
    derivingClasses = genDerivingClasses group

genDerivingClasses :: Types.Group -> LT.Text
genDerivingClasses group
    | Types.groupMemberType group == Types.GLbitfield = "Show, Eq, Read, Bits"
    | otherwise = "Show, Eq, Read"

genGroupMemberDeclaresCode :: Types.Group -> LT.Text
genGroupMemberDeclaresCode group =
    [lt|{-# OPTIONS -fno-warn-unused-imports #-}
module GLW.Groups.#{groupName}
    ( #{groupName}
    , #{(T.intercalate "\n    , " . map toLowerCamelCase) groupMembers}
    ) where

import GLW.Internal.Groups (#{groupName}(..))
import qualified Graphics.GL as GL
import qualified Graphics.GL.Ext as GL
import qualified Graphics.GL.Internal.Shared as GL

#{LT.intercalate "\n" memberDeclares}|]
    where
    groupName = Types.groupName group
    groupMembers = Types.groupMembers group
    memberDeclares = map (genGroupMemberDeclare groupName) groupMembers

genGroupMemberDeclare :: T.Text -> T.Text -> LT.Text
genGroupMemberDeclare groupName memberName =
    let memberName' = toLowerCamelCase memberName
    in [lt|#{memberName'} :: #{groupName}
#{memberName'} = #{groupName} GL.#{memberName}
|]

writeGroupMemberDeclaresCodes :: FilePath -> [Types.Group] -> IO ()
writeGroupMemberDeclaresCodes outputPath =
    mapM_ (writeGroupMemberDeclaresCode outputPath)

writeGroupMemberDeclaresCode :: FilePath -> Types.Group -> IO ()
writeGroupMemberDeclaresCode outputPath group =
    let groupName = Types.groupName group
        code = genGroupMemberDeclaresCode group
        path = (outputPath ++) . T.unpack . T.concat $ ["/GLW/Groups/", groupName, ".hs"]
    in LT.writeFile path code

toLowerCamelCase :: T.Text -> T.Text
toLowerCamelCase = T.concat . zipWith f ([0..] :: [Int]) . T.splitOn "_"
    where
    f 0 t = T.toLower t
    f _ t = T.toTitle t
