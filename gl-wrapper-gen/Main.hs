{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Command (parseCommand, writeAll)
import Control.Exception (throwIO)
import Control.Lens (filtered, ix)
import Data.Maybe (maybe)
import qualified Data.Text.Lazy.IO as LT (putStr)
import Debug.Trace (trace)
import qualified Group (parseGroup, writeAll)
import System.Directory (createDirectoryIfMissing)
import System.IO.Error (userError)
import qualified Text.XML as XML (Node(..), def, readFile)
import Text.XML.Lens
import Types (Group(..))

main :: IO ()
main = do
    doc <- XML.readFile XML.def "gl.xml"
    createDirectoryIfMissing True "gl-wrapper/GLW/Internal"
    let groupElements = doc ^.. root . el "registry" ./ el "groups" ./ el "group"
        maybeGroups = mapM Group.parseGroup groupElements
        --commandElements = doc ^.. root . el "registry" ./ el "commands" ./ el "command" . filtered (\x -> x ^? el "command" ./ el "proto" ./ el "name" . text == Just "glAreTexturesResident")
        commandElements = doc ^.. root . el "registry" ./ el "commands" ./ el "command"
    groups <- maybe (throwIO . userError $ "failed to parse groups") return maybeGroups
    let groupNames = map Types.groupName groups
    -- Group.writeAll groups

    let maybeCommands = mapM (Command.parseCommand groupNames) commandElements
    commands <- maybe (throwIO . userError $ "failed to parse commands") return maybeCommands
    Command.writeAll groupNames commands
