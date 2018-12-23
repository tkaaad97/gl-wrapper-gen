{-# LANGUAGE OverloadedStrings #-}
module Command
    ( parseCommand
    ) where

import qualified Data.Text as T (Text, concat, filter, intercalate, length,
                                 unpack)
import qualified Data.Text.Lazy as LT (Text, intercalate)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Text.Read (readMaybe)
import Text.Shakespeare.Text (lt)
import qualified Text.XML as XML (Element(..), Node(..))
import Text.XML.Lens
import qualified Types

parseCommand ::  XML.Element -> Maybe Types.Command
parseCommand elem = do
    name <- elem ^?  el "command" ./ el "proto" ./ el "name" . text
    ptype <-parseTypeInfo =<< elem ^? el "command" ./ el "proto"
    params <- mapM parseParam $ elem ^.. el "command" ./ el "param"
    return (Types.Command name params ptype)

parseParam ::  XML.Element -> Maybe Types.Param
parseParam elem = do
    name <- elem ^?  entire ./ el "name" . text
    ptype <- parseTypeInfo elem
    return (Types.Param name ptype)

parseTypeInfo :: XML.Element -> Maybe Types.TypeInfo
parseTypeInfo a = do
    ptype <- parsePrimType (a ^? entire ./ el "ptype" . text)
    let type' = handlePtr (Types.TypePtr (Types.TypePrim ptype)) pointer
    return (Types.TypeInfo type' group len)
    where
    pointer = T.length . T.filter ('*' ==) . T.concat $ a ^.. entire . text
    group = a ^? attr "group"
    len = a ^? attr "len"

parsePrimType :: Maybe T.Text -> Maybe Types.PrimType
parsePrimType (Just p) = readMaybe . T.unpack $ p
parsePrimType Nothing  = Just Types.Void

handlePtr :: Types.Type -> Int -> Types.Type
handlePtr p n | n <= 0 = p
handlePtr p n = handlePtr (Types.TypePtr p) (n - 1)
