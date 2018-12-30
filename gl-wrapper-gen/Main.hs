{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Command (parseCommand, writeAll)
import Control.Arrow ((&&&))
import Control.Exception (throwIO)
import Control.Lens (filtered, ix)
import qualified Data.Map.Strict as Map (fromList)
import Data.Maybe (isJust, maybe)
import Data.Set (Set)
import qualified Data.Set as Set (difference, fromList, member, union)
import qualified Data.Text.Lazy.IO as LT (putStr)
import qualified Group (parseGroup, parseGroupMemberTypes, writeAll)
import qualified Object (parseObject, writeObjectDeclaresCode)
import System.Directory (createDirectoryIfMissing)
import System.IO.Error (userError)
import qualified Text.XML as XML (Node(..), def, readFile)
import Text.XML.Lens
import qualified Types (Command(..), Group(..))

main :: IO ()
main = do
    doc <- XML.readFile XML.def "gl.xml"
    objectDoc <- XML.readFile XML.def "objects.xml"
    createDirectoryIfMissing True "gl-wrapper/GLW/Internal"
    createDirectoryIfMissing True "gl-wrapper/GLW/Groups"
    let featureEnumNames = Set.fromList $ doc ^.. root . el "registry" ./ el "feature" ./ el "require" ./ el "enum" . attr "name"
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
    Group.writeAll filteredGroups

    let maybeCommands = mapM (Command.parseCommand groupNames) commandElements
    commands <- maybe (throwIO . userError $ "failed to parse commands") return maybeCommands
    Command.writeAll groupNames commands

    let objectElements = objectDoc ^.. root . el "objects" ./ el "object"
        cm = Map.fromList . map (Types.commandName &&& id) $ commands
        maybeObjects = mapM (Object.parseObject cm) objectElements
    objects <- maybe (throwIO . userError $ "failed to parse objects") return maybeObjects
    Object.writeObjectDeclaresCode objects
    return ()

    where
    filterCommand commandNames x = isJust $
        x ^? el "command" ./ el "proto" ./ el "name" . text . filtered (`Set.member` commandNames)
