{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Lazy.IO as LT (putStr)
import Group (genGroupCode, parseGroup)
import qualified Text.XML as XML (def, readFile)
import Text.XML.Lens
import Types (Group(..))

main :: IO ()
main = do
    doc <- XML.readFile XML.def "gl.xml"
    let group = head $ doc ^.. root . el "registry" ./ el "groups" ./ el "group"
        Just code = genGroupCode <$> parseGroup group
    LT.putStr code
