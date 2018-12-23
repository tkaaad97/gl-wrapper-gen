{-# LANGUAGE OverloadedStrings #-}
module Main where

import Command (parseCommand)
import Control.Exception (throwIO)
import Control.Lens (filtered, ix)
import Data.Maybe (maybe)
import qualified Data.Text.Lazy.IO as LT (putStr)
import Debug.Trace (trace)
import qualified Group (parseGroup, writeAll)
import System.IO.Error (userError)
import qualified Text.XML as XML (Node(..), def, readFile)
import Text.XML.Lens
import Types (Group(..))

main :: IO ()
main = do
    doc <- XML.readFile XML.def "gl.xml"
    let groupElements = doc ^.. root . el "registry" ./ el "groups" ./ el "group"
        maybeGroups = mapM Group.parseGroup groupElements
        commandElements = doc ^.. root . el "registry" ./ el "commands" ./ el "command" . filtered (\x -> x ^? el "command" ./ el "proto" ./ el "name" . text == Just "glAreTexturesResident")
        --commandElements = doc ^.. root . el "registry" ./ el "commands" ./ el "command"
    groups <- maybe (throwIO . userError $ "failed to parse groups") return maybeGroups
    -- Group.writeAll groups
    print (map parseCommand commandElements)
