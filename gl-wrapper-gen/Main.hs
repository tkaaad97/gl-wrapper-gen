{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Command (parseCommand, writeAll)
import Control.Arrow ((&&&))
import Control.Exception (throwIO)
import Control.Lens (filtered)
import qualified Data.Map.Strict as Map (fromList)
import Data.Maybe (isJust, maybe)
import qualified Data.Set as Set (difference, fromList, member, union)
import qualified Group (parseGroup, parseGroupMemberTypes, writeAll)
import qualified Newtype (parseNewtype, writeNewtypeDeclaresCode)
import qualified Object (parseObject, writeObjectDeclaresCode)
import System.Directory (createDirectoryIfMissing)
import System.IO.Error (userError)
import qualified Text.XML as XML (def, readFile)
import Text.XML.Lens
import qualified Types (Group(..), Newtype(..), Object(..))
import qualified Uniform

main :: IO ()
main = do
    doc <- XML.readFile XML.def "glw.xml"
    objectDoc <- XML.readFile XML.def "objects.xml"
    newtypeDoc <- XML.readFile XML.def "newtypes.xml"
    createDirectoryIfMissing True "gl-wrapper/GLW/Internal"
    createDirectoryIfMissing True "gl-wrapper/GLW/Groups"

    let objectElements = objectDoc ^.. root . el "objects" ./ el "object"
        maybeObjects = mapM Object.parseObject objectElements
    objects <- maybe (throwIO . userError $ "failed to parse objects") return maybeObjects

    let newtypeElements = newtypeDoc ^.. root . el "newtypes" ./ el "newtype"
        maybeNewtypes = mapM Newtype.parseNewtype newtypeElements
    newtypes <- maybe (throwIO . userError $ "failed to parse objects") return maybeNewtypes

    let om = Map.fromList . map (Types.objectName &&& id) $ objects
        nm = Map.fromList . map (Types.newtypeName &&& id) $ newtypes
        featureEnumNames = Set.fromList $ doc ^.. root . el "registry" ./ el "feature" ./ el "require" ./ el "enum" . attr "name"
        extEnumNames = Set.fromList $ doc ^.. root . el "registry" ./ el "extensions" ./ el "extension" ./ el "require" ./ el "enum" . attr "name"
        enumNames = Set.union featureEnumNames extEnumNames
        groupElements = doc ^.. root . el "registry" ./ el "groups" ./ el "group"
        groupMemberTypes = Group.parseGroupMemberTypes doc
        maybeGroups = mapM (Group.parseGroup enumNames groupMemberTypes) groupElements
        glFeatureCommandNames = Set.fromList $ doc ^.. root . el "registry" ./ el "feature" . attributeIs "api" "gl" ./ el "require" ./ el "command" . attr "name"
        removedCommandNames = Set.fromList $ doc ^.. root . el "registry" ./ el "feature" ./ el "remove" ./ el "command" . attr "name"
        extensionCommandNames = Set.fromList $ doc ^.. root . el "registry" ./ el "extensions" ./ el "extension" . attributeIs "supported" "gl" ./ el "require" ./ el "command" . attr "name"
        commandNames = (glFeatureCommandNames `Set.union` extensionCommandNames) `Set.difference` removedCommandNames
        commandElements = doc ^.. root . el "registry" ./ el "commands" ./ el "command" . filtered (filterCommand commandNames)

    groups <- maybe (throwIO . userError $ "failed to parse groups") return maybeGroups
    let filteredGroups = filter (not . null . Types.groupMembers)  groups
        groupNames = Set.fromList $ map Types.groupName filteredGroups

    let maybeCommands = mapM (Command.parseCommand groupNames om nm) commandElements
    commands <- maybe (throwIO . userError $ "failed to parse commands") return maybeCommands

    Object.writeObjectDeclaresCode objects
    Newtype.writeNewtypeDeclaresCode newtypes
    Group.writeAll filteredGroups
    Command.writeAll groupNames om commands
    Uniform.writeUniformCode

    return ()

    where
    filterCommand commandNames x = isJust $
        x ^? el "command" ./ el "proto" ./ el "name" . text . filtered (`Set.member` commandNames)
