{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Command
    ( parseCommand
    , writeAll
    ) where

import Control.Lens (filtered)
import qualified Data.List as List (elem)
import Data.Maybe (isJust)
import qualified Data.Text as T (Text, concat, filter, intercalate, length,
                                 pack, unpack)
import qualified Data.Text.Lazy as LT (Text, intercalate)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Text.Read (readMaybe)
import Text.Shakespeare.Text (lt, st)
import qualified Text.XML as XML (Element(..), Node(..))
import Text.XML.Lens
import qualified Types

parseCommand :: [T.Text] -> XML.Element -> Maybe Types.Command
parseCommand enumGroupNames elem = do
    name <- elem ^?  el "command" ./ el "proto" ./ el "name" . text
    ptype <-parseTypeInfo enumGroupNames =<< elem ^? el "command" ./ el "proto"
    params <- mapM (parseParam enumGroupNames) $ elem ^.. el "command" ./ el "param"
    return (Types.Command name params ptype)

parseParam :: [T.Text] -> XML.Element -> Maybe Types.Param
parseParam enumGroupNames elem = do
    name <- elem ^?  entire ./ el "name" . text
    ptype <- parseTypeInfo enumGroupNames elem
    return (Types.Param name ptype)

parseTypeInfo :: [T.Text] -> XML.Element -> Maybe Types.TypeInfo
parseTypeInfo enumGroupNames a = do
    ptype <- parsePrimType (a ^? entire ./ el "ptype" . text)
    let type' = handlePtr (Types.TypePtr (Types.TypePrim ptype)) pointer
    return (Types.TypeInfo type' group len)
    where
    pointer = T.length . T.filter ('*' ==) . T.concat $ a ^.. entire . text
    group = a ^? attr "group" . filtered (`List.elem` enumGroupNames)
    len = a ^? attr "len"

parsePrimType :: Maybe T.Text -> Maybe Types.PrimType
parsePrimType (Just p) = readMaybe . T.unpack $ p
parsePrimType Nothing  = Just Types.Void

handlePtr :: Types.Type -> Int -> Types.Type
handlePtr p n | n <= 0 = p
handlePtr p n = handlePtr (Types.TypePtr p) (n - 1)

genCommandDeclaresCode :: [T.Text] -> [Types.Command] -> LT.Text
genCommandDeclaresCode groupNames commands =
    let commandNames = map Types.commandName commands
        commandDeclares = map genCommandDeclare commands
    in [lt|module GLW
    ( #{T.intercalate "\n    , " groupNames}
    , #{T.intercalate "\n    , " commandNames}
    ) where

import Data.Coerce (coerce)
import qualified Graphics.GL.Internal.Shared as GL
import GLW.Internal.Enums

#{LT.intercalate "\n" commandDeclares}|]

genCommandDeclare :: Types.Command -> LT.Text
genCommandDeclare command =
    let commandName = Types.commandName command
        params = Types.commandParams command
        paramNames = map Types.paramName params
        commandSignature = genCommandSignature command
        coerceParams = map genCoerceParam params
    in [lt|#{commandName} :: #{commandSignature}
#{commandName} #{T.intercalate " " paramNames} = GL.#{commandName} #{T.intercalate " " coerceParams}
|]

genCommandSignature :: Types.Command -> T.Text
genCommandSignature command =
    let params = Types.commandParams command
        ptypes = map (genParamType . Types.paramTypeInfo) params
        rtype = genParamType . Types.commandReturnTypeInfo $ command
    in T.intercalate " -> " (ptypes ++ [rtype])

genParamType :: Types.TypeInfo -> T.Text
genParamType a = genType type' gorup
    where
    type' = Types.typeInfoType a
    gorup = Types.typeInfoEnumGroup a

genType :: Types.Type -> Maybe T.Text -> T.Text
genType (Types.TypePrim t) Nothing               = [st|GL.#{show t}|]
genType (Types.TypePrim _) (Just groupName)      = groupName
genType (Types.TypePtr a @ (Types.TypePtr _)) g  = [st|Ptr (#{genType a g})|]
genType (Types.TypePtr a @ (Types.TypePrim _)) g = [st|Ptr #{genType a g}|]

genCoerceParam :: Types.Param -> T.Text
genCoerceParam (Types.Param name typeInfo) | isJust (Types.typeInfoEnumGroup typeInfo) = [st|(coerce #{name})|]
genCoerceParam (Types.Param name _) = name

writeAll :: [T.Text] -> [Types.Command] -> IO ()
writeAll groupNames commands =
    let code = genCommandDeclaresCode groupNames commands
        path = "gl-wrapper/GLW.hs"
    in LT.writeFile path code
