{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Command
    ( parseCommand
    , writeAll
    ) where

import Control.Lens (filtered)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set (member, toList)
import qualified Data.Text as T (Text, concat, filter, intercalate, length,
                                 pack, unpack)
import qualified Data.Text.Lazy as LT (Text, intercalate)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Text.Read (readMaybe)
import Text.Shakespeare.Text (lt, st)
import qualified Text.XML as XML (Element(..), Node(..))
import Text.XML.Lens
import qualified Types

parseCommand :: Set T.Text -> XML.Element -> Maybe Types.Command
parseCommand enumGroupNames elem = do
    name <- elem ^?  el "command" ./ el "proto" ./ el "name" . text
    ptype <-parseTypeInfo enumGroupNames =<< elem ^? el "command" ./ el "proto"
    params <- mapM (parseParam enumGroupNames) $ elem ^.. el "command" ./ el "param"
    return (Types.Command name params ptype)

parseParam :: Set T.Text -> XML.Element -> Maybe Types.Param
parseParam enumGroupNames elem = do
    name <- elem ^?  entire ./ el "name" . text
    ptype <- parseTypeInfo enumGroupNames elem
    return (Types.Param name ptype)

parseTypeInfo :: Set T.Text -> XML.Element -> Maybe Types.TypeInfo
parseTypeInfo enumGroupNames a = do
    ptype <- Types.parsePrimType (a ^? entire ./ el "ptype" . text)
    let type' = handlePtr (Types.TypePrim ptype) pointer
    return (Types.TypeInfo type' group len)
    where
    pointer = T.length . T.filter ('*' ==) . T.concat $ a ^.. entire . text
    group = a ^? attr "group" . filtered (`Set.member` enumGroupNames)
    len = a ^? attr "len"

handlePtr :: Types.Type -> Int -> Types.Type
handlePtr p n | n <= 0 = p
handlePtr p n = handlePtr (Types.TypePtr p) (n - 1)

genCommandDeclaresCode :: Set T.Text -> [Types.Command] -> LT.Text
genCommandDeclaresCode groupNames commands =
    let commandNames = map Types.commandName commands
        commandDeclares = map genCommandDeclare commands
    in [lt|module GLW
    ( #{T.intercalate "\n    , " (Set.toList groupNames)}(..)
    , #{T.intercalate "\n    , " commandNames}
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Foreign.Ptr (Ptr)
import qualified Graphics.GL as GL
import qualified Graphics.GL.Compatibility45 as GL
import qualified Graphics.GL.Ext as GL
import qualified Graphics.GL.Internal.Shared as GL
import GLW.Internal.Groups

#{LT.intercalate "\n" commandDeclares}|]

genCommandDeclare :: Types.Command -> LT.Text
genCommandDeclare command =
    let commandName = Types.commandName command
        params = Types.commandParams command
        paramNames = map (modifyParamName . Types.paramName) params
        commandSignature = genCommandSignature command
        coerceParams = map genCoerceParam params
        coerceReturnType = genCoerceReturn . Types.commandReturnTypeInfo $ command
    in [lt|#{commandName} :: #{commandSignature}
#{commandName} #{T.intercalate " " paramNames} = #{coerceReturnType}GL.#{commandName} #{T.intercalate " " coerceParams}
|]

genCommandSignature :: Types.Command -> T.Text
genCommandSignature command =
    let params = Types.commandParams command
        ptypes = map (genParamType . Types.paramTypeInfo) params
        rtype = genReturnType . Types.commandReturnTypeInfo $ command
    in T.concat ["MonadIO m => ", T.intercalate " -> " (ptypes ++ [rtype])]

genParamType :: Types.TypeInfo -> T.Text
genParamType a = genType type' gorup
    where
    type' = Types.typeInfoType a
    gorup = Types.typeInfoEnumGroup a

genReturnType :: Types.TypeInfo -> T.Text
genReturnType a @ (Types.TypeInfo (Types.TypePtr _) _ _) = T.concat ["m (", genType (Types.typeInfoType a) (Types.typeInfoEnumGroup a), ")"]
genReturnType a = T.concat ["m ", genType (Types.typeInfoType a) (Types.typeInfoEnumGroup a)]

genType :: Types.Type -> Maybe T.Text -> T.Text
genType (Types.TypePrim t) Nothing               = [st|#{Types.printPrimType "GL." t}|]
genType (Types.TypePrim _) (Just groupName)      = groupName
genType (Types.TypePtr a @ (Types.TypePtr _)) g  = [st|Ptr (#{genType a g})|]
genType (Types.TypePtr a @ (Types.TypePrim _)) g = [st|Ptr #{genType a g}|]

genCoerceParam :: Types.Param -> T.Text
genCoerceParam (Types.Param name typeInfo) | isJust (Types.typeInfoEnumGroup typeInfo) = [st|(coerce #{modifyParamName name})|]
genCoerceParam (Types.Param name _) = modifyParamName name

genCoerceReturn :: Types.TypeInfo -> T.Text
genCoerceReturn (Types.TypeInfo _ (Just _) _) = "coerce <$> "
genCoerceReturn _                             = ""

modifyParamName :: T.Text -> T.Text
modifyParamName name | name == "type" = "type'"
modifyParamName name | name == "data" = "data'"
modifyParamName name | name == "in" = "in'"
modifyParamName name = name

writeAll :: Set T.Text -> [Types.Command] -> IO ()
writeAll groupNames commands =
    let code = genCommandDeclaresCode groupNames commands
        path = "gl-wrapper/GLW.hs"
    in LT.writeFile path code
