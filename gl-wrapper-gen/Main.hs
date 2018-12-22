{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (throwIO)
import Data.Maybe (maybe)
import qualified Data.Text.Lazy.IO as LT (putStr)
import qualified Group (parseGroup, writeAll)
import System.IO.Error (userError)
import qualified Text.XML as XML (def, readFile)
import Text.XML.Lens
import Types (Group(..))

main :: IO ()
main = do
    doc <- XML.readFile XML.def "gl.xml"
    let groupElements = doc ^.. root . el "registry" ./ el "groups" ./ el "group"
        maybeGroups = mapM Group.parseGroup groupElements
    groups <- maybe (throwIO . userError $ "failed to parse groups") return maybeGroups
    Group.writeAll groups
