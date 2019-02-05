{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Command
    ( parseCommand
    , writeAll
    ) where

import Control.Lens (filtered)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, keys, lookup)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (member, toList)
import qualified Data.Text as T (Text, concat, filter, intercalate, length)
import qualified Data.Text.Lazy as LT (Text, intercalate)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import qualified Text.HTML.TagSoup as TagSoup (fromTagText, parseTags)
import Text.Shakespeare.Text (lt, st)
import qualified Text.XML as XML (Element(..))
import Text.XML.Lens
import qualified Types

parseCommand :: Set T.Text -> Map T.Text Types.Object -> Map T.Text Types.Newtype -> XML.Element -> Maybe Types.Command
parseCommand enumGroupNames objects newtypes elem' = do
    cname <- elem' ^?  el "command" ./ el "proto" ./ el "name" . text
    ptype <-parseReturnTypeInfo enumGroupNames objects newtypes =<< elem' ^? el "command" ./ el "proto"
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

parseReturnTypeInfo :: Set T.Text -> Map T.Text Types.Object -> Map T.Text Types.Newtype -> XML.Element -> Maybe Types.ReturnTypeInfo
parseReturnTypeInfo enumGroupNames objects newtypes a = do
    t <- parseTypeInfo enumGroupNames objects newtypes a
    return (Types.ReturnTypeInfo t validation)
    where
    validation = a ^? attr "validation"

handlePtr :: Types.Type -> Int -> Types.Type
handlePtr p n | n <= 0 = p
handlePtr p n = handlePtr (Types.TypePtr p) (n - 1)

genExportCode :: Set T.Text -> Map T.Text Types.Object -> LT.Text
genExportCode groupNames objects =
    [lt|module GLW
    ( #{T.intercalate "\n    , " (Set.toList groupNames)}
    , #{T.intercalate "\n    , " objectNames}
    , #{T.intercalate "(..)\n    , " discriminatorNames}(..)
    , Sing#{T.intercalate "\n    , Sing" discriminatorNames}
    , Object(..)
    , module GLW.Commands
    , module GLW.Types
    , module GLW.Uniforms
    ) where

import GLW.Commands
import GLW.Internal.Groups
import GLW.Internal.Objects
import GLW.Types
import GLW.Uniforms
|]
    where
    objectNames = Map.keys objects
    discriminatorNames = mapMaybe (fmap Types.objectDiscriminatorName . Types.objectDiscriminator) . Map.elems $ objects

genCommandDeclaresCode :: [Types.Command] -> LT.Text
genCommandDeclaresCode commands =
    [lt|{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module GLW.Commands
    ( #{T.intercalate "\n    , " commandNames}
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Foreign.Ptr (Ptr)
import qualified Graphics.GL as GL
import qualified Graphics.GL.Compatibility45 as GL
import qualified Graphics.GL.Ext as GL
import GLW.Internal.Groups
import GLW.Internal.ObjectTypes
import GLW.Types
#ifdef GLW_DEBUG
import GLW.Debug
#endif
import Prelude (Eq(..), Maybe, Ord(..), (.), (<$>), fmap, fromIntegral, return)

#{LT.intercalate "\n" commandDeclares}|]
    where
    commandNames = map Types.commandName commands
    commandDeclares = map genCommandDeclare commands

genCommandDeclare :: Types.Command -> LT.Text
genCommandDeclare command =
    [lt|#{commandName} :: #{commandSignature}
#{commandName} #{T.intercalate " " paramNames} = do
#ifdef GLW_DEBUG
    #{logCommandStart}
#endif
    _result <- #{validationReturnType}GL.#{commandName} #{T.intercalate " " coerceParams}
#ifdef GLW_DEBUG
    #{logCommandEnd}
#endif
    return _result
|]
    where
    commandName = Types.commandName command
    params = Types.commandParams command
    paramNames = map (modifyParamName . Types.paramName) params
    commandSignature = genCommandSignature command
    coerceParams = map genCoerceParam params
    validationReturnType = genValidationReturn . Types.commandReturnTypeInfo $ command
    returnType = Types.typeInfoType . Types.returnTypeInfoTypeInfo . Types.commandReturnTypeInfo $ command
    logCommandStart = genLogCommandStart commandName (zip paramNames params)
    logCommandEnd = genLogCommandEnd commandName returnType

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

genReturnType :: Types.ReturnTypeInfo -> T.Text
genReturnType (Types.ReturnTypeInfo a validation) = "m " <> rt validation
    where
    rt Nothing  = gen True
    rt (Just _) = "(Maybe " <> gen False <> ")"
    gen b = genType b (Types.typeInfoType a) (Types.typeInfoEnumGroup a) (Types.typeInfoObject a) (Types.typeInfoNewtype a)

genType :: Bool -> Types.Type -> Maybe T.Text -> Maybe Types.Object -> Maybe Types.Newtype -> T.Text
genType _ (Types.TypePrim t) Nothing Nothing Nothing = [st|#{Types.printPrimType "GL." t}|]
genType _ (Types.TypePrim _) (Just groupName) _ _    = groupName
genType b (Types.TypePrim _) Nothing (Just object) _ = genObjectType b object
genType _ (Types.TypePrim _) Nothing Nothing (Just ntype) = Types.newtypeName ntype
genType b (Types.TypePtr a) g o n = handleBracket b [st|Ptr #{genType True a g o n}|]

genConstructor :: Types.TypeInfo -> T.Text
genConstructor (Types.TypeInfo _ (Just group) _ _ _)  = group
genConstructor (Types.TypeInfo _ _ (Just object) _ _) = Types.objectName object
genConstructor (Types.TypeInfo _ _ _ (Just ntype) _)  = Types.newtypeName ntype
genConstructor _                                      = "coerce"

handleBracket :: Bool -> T.Text -> T.Text
handleBracket True a = T.concat ["(", a, ")"]
handleBracket _ a    = a

genObjectType :: Bool -> Types.Object -> T.Text
genObjectType _ (Types.Object oname _ _ Nothing) = oname
genObjectType b (Types.Object oname _ _ (Just (Types.ObjectDiscriminator dname _))) = handleBracket b [st|#{oname} (a :: #{dname})|]

genCoerceParam :: Types.Param -> T.Text
genCoerceParam (Types.Param pname typeInfo)
    | needCoerce typeInfo = [st|(coerce #{modifyParamName pname})|]
    | otherwise = modifyParamName pname

genValidationReturn :: Types.ReturnTypeInfo -> T.Text
genValidationReturn (Types.ReturnTypeInfo t Nothing)
    | needCoerce t = "coerce <$> "
    | otherwise = ""
genValidationReturn (Types.ReturnTypeInfo t (Just cond))
    | needCoerce t = [st|fmap (#{constructor} . fromIntegral) . validate (#{cond'}) <$> |]
    | otherwise = [st|validate (#{cond'}) <$> |]
    where
    cond' = TagSoup.fromTagText . head . TagSoup.parseTags $ cond
    constructor = genConstructor t

genLogCommandStart :: T.Text -> [(T.Text, Types.Param)] -> LT.Text
genLogCommandStart commandName params =
    [lt|logGLCommandStart "#{commandName}" [#{logParams}]|]
    where
    logParams = T.intercalate ", " (map genLogParam params)

genLogCommandEnd :: T.Text -> Types.Type -> LT.Text
genLogCommandEnd commandName returnType =
    [lt|logGLCommandEnd "#{commandName}" (#{resultValue})|]
    where
    resultValue = genLogV "_result" returnType

genLogParam :: (T.Text, Types.Param) -> T.Text
genLogParam (paramName, param) = genLogV paramName type'
    where
    type' = Types.typeInfoType . Types.paramTypeInfo $ param

genLogV :: T.Text -> Types.Type -> T.Text
genLogV _ (Types.TypePrim Types.Void) = "LogV'Void"
genLogV paramName (Types.TypePrim primType) = T.concat [Types.printPrimType "LogV'" primType, " ", paramName]
genLogV _ (Types.TypePtr _) = "LogV'Ptr"

needCoerce :: Types.TypeInfo -> Bool
needCoerce (Types.TypeInfo _ (Just _) _ _ _) = True
needCoerce (Types.TypeInfo _ _ (Just _) _ _) = True
needCoerce (Types.TypeInfo _ _ _ (Just _) _) = True
needCoerce _                                 = False

modifyParamName :: T.Text -> T.Text
modifyParamName pname | pname == "type" = "type'"
modifyParamName pname | pname == "data" = "data'"
modifyParamName pname | pname == "in" = "in'"
modifyParamName pname = pname

writeAll :: FilePath -> Set T.Text -> Map T.Text Types.Object -> [Types.Command] -> IO ()
writeAll outputPath groupNames objects commands = do
    LT.writeFile (outputPath ++ "/GLW/Commands.hs") commandCode
    LT.writeFile (outputPath ++ "/GLW.hs") exportCode
    where
    commandCode = genCommandDeclaresCode commands
    exportCode = genExportCode groupNames objects
