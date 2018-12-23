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
    ptype <-parseParamType =<< elem ^? el "command" ./ el "proto"
    params <- mapM parseParam $ elem ^.. el "command" ./ el "param"
    return (Types.Command name params ptype)

parseParam ::  XML.Element -> Maybe Types.Param
parseParam elem = do
    name <- elem ^?  entire ./ el "name" . text
    ptype <- parseParamType elem
    return (Types.Param name ptype)

parseParamType :: XML.Element -> Maybe Types.ParamType
parseParamType a = do
    ptype <- parsePrimType (a ^? entire ./ el "ptype" . text)
    return $ handlePtr (Types.ParamTypePtr (Types.ParamTypePrim ptype)) pointer
    where
    pointer = T.length . T.filter ('*' ==) . T.concat $ a ^.. entire . text

parsePrimType :: Maybe T.Text -> Maybe Types.PrimType
parsePrimType (Just p) = readMaybe . T.unpack $ p
parsePrimType Nothing  = Just Types.Void

handlePtr :: Types.ParamType -> Int -> Types.ParamType
handlePtr p n | n <= 0 = p
handlePtr p n = handlePtr (Types.ParamTypePtr p) (n - 1)
