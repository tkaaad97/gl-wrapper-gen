{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Command
    ( parseCommand
    , writeAll
    ) where

import Control.Lens (filtered)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, keys, lookup)
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (member, toList)
import qualified Data.Text as T (Text, concat, filter, intercalate, length)
import qualified Data.Text.Lazy as LT (Text, intercalate)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Text.Shakespeare.Text (lt, st)
import qualified Text.XML as XML (Element(..))
import Text.XML.Lens
import qualified Types

parseCommand :: Set T.Text -> Map T.Text Types.Object -> Map T.Text Types.Newtype -> XML.Element -> Maybe Types.Command
parseCommand enumGroupNames objects newtypes elem' = do
    cname <- elem' ^?  el "command" ./ el "proto" ./ el "name" . text
    ptype <-parseTypeInfo enumGroupNames objects newtypes =<< elem' ^? el "command" ./ el "proto"
    params <- mapM (parseParam enumGroupNames objects newtypes) $ elem' ^.. el "command" ./ el "param"
    return (Types.Command cname params ptype)

parseParam :: Set T.Text -> Map T.Text Types.Object -> Map T.Text Types.Newtype -> XML.Element -> Maybe Types.Param
parseParam enumGroupNames objects newtypes elem' = do
    pname <- elem' ^?  entire ./ el "name" . text
    ptype <- parseTypeInfo enumGroupNames objects newtypes elem'
    return (Types.Param pname ptype)

parseTypeInfo :: Set T.Text -> Map T.Text Types.Object -> Map T.Text Types.Newtype -> XML.Element -> Maybe Types.TypeInfo
parseTypeInfo enumGroupNames objects newtypes a = do
    ptype <- Types.parsePrimType (a ^? entire ./ el "ptype" . text)
    let type' = handlePtr (Types.TypePrim ptype) pointer
    return (Types.TypeInfo type' group object ntype len)
    where
    pointer = T.length . T.filter ('*' ==) . T.concat $ a ^.. entire . text
    group = a ^? attr "group" . filtered (`Set.member` enumGroupNames)
    object = a ^? attr "object" >>= (`Map.lookup` objects)
    ntype = a ^? attr "newtype" >>= (`Map.lookup` newtypes)
    len = a ^? attr "len"

handlePtr :: Types.Type -> Int -> Types.Type
handlePtr p n | n <= 0 = p
handlePtr p n = handlePtr (Types.TypePtr p) (n - 1)

genCommandDeclaresCode :: Set T.Text -> Map T.Text Types.Object ->  [Types.Command] -> LT.Text
genCommandDeclaresCode groupNames objects commands =
    let commandNames = map Types.commandName commands
        commandDeclares = map genCommandDeclare commands
        objectNames = Map.keys objects
        discriminatorNames = mapMaybe (fmap Types.objectDiscriminatorName . Types.objectDiscriminator) . Map.elems $ objects
    in [lt|{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module GLW
    ( #{T.intercalate "\n    , " (Set.toList groupNames)}
    , #{T.intercalate "\n    , " objectNames}
    , #{T.intercalate "(..)\n    , " discriminatorNames}(..)
    , #{T.intercalate "\n    , " commandNames}
    , module GLW.Types
    , module GLW.Uniforms
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Foreign.Ptr (Ptr)
import qualified Graphics.GL as GL
import qualified Graphics.GL.Compatibility45 as GL
import qualified Graphics.GL.Ext as GL
import GLW.Internal.Groups
import GLW.Internal.Objects
import GLW.Types
import GLW.Uniforms
import Prelude ((<$>))

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
genParamType a = genType False type' gorup object ntype
    where
    type' = Types.typeInfoType a
    gorup = Types.typeInfoEnumGroup a
    object = Types.typeInfoObject a
    ntype = Types.typeInfoNewtype a

genReturnType :: Types.TypeInfo -> T.Text
genReturnType a = T.concat ["m ", genType True (Types.typeInfoType a) (Types.typeInfoEnumGroup a) (Types.typeInfoObject a) (Types.typeInfoNewtype a)]

genType :: Bool -> Types.Type -> Maybe T.Text -> Maybe Types.Object -> Maybe Types.Newtype -> T.Text
genType _ (Types.TypePrim t) Nothing Nothing Nothing = [st|#{Types.printPrimType "GL." t}|]
genType _ (Types.TypePrim _) (Just groupName) _ _    = groupName
genType b (Types.TypePrim _) Nothing (Just object) _ = genObjectType b object
genType _ (Types.TypePrim _) Nothing Nothing (Just ntype) = Types.newtypeName ntype
genType b (Types.TypePtr a) g o n = handleBracket b [st|Ptr #{genType True a g o n}|]

handleBracket :: Bool -> T.Text -> T.Text
handleBracket True a = T.concat ["(", a, ")"]
handleBracket _ a    = a

genObjectType :: Bool -> Types.Object -> T.Text
genObjectType _ (Types.Object oname _ _ Nothing) = oname
genObjectType b (Types.Object oname _ _ (Just (Types.ObjectDiscriminator dname _))) = handleBracket b [st|#{oname} (a :: #{dname})|]

genCoerceParam :: Types.Param -> T.Text
genCoerceParam (Types.Param pname typeInfo) | isJust (Types.typeInfoEnumGroup typeInfo) || isJust (Types.typeInfoObject typeInfo) || isJust (Types.typeInfoNewtype typeInfo) =
    [st|(coerce #{modifyParamName pname})|]
genCoerceParam (Types.Param pname _) = modifyParamName pname

genCoerceReturn :: Types.TypeInfo -> T.Text
genCoerceReturn (Types.TypeInfo _ (Just _) _ _ _) = "coerce <$> "
genCoerceReturn (Types.TypeInfo _ _ (Just _) _ _) = "coerce <$> "
genCoerceReturn (Types.TypeInfo _ _ _ (Just _) _) = "coerce <$> "
genCoerceReturn _                                 = ""

modifyParamName :: T.Text -> T.Text
modifyParamName pname | pname == "type" = "type'"
modifyParamName pname | pname == "data" = "data'"
modifyParamName pname | pname == "in" = "in'"
modifyParamName pname = pname

writeAll :: FilePath -> Set T.Text -> Map T.Text Types.Object -> [Types.Command] -> IO ()
writeAll outputPath groupNames objects commands =
    let code = genCommandDeclaresCode groupNames objects commands
        path = outputPath ++ "/GLW.hs"
    in LT.writeFile path code
