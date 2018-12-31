{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Object
    ( parseObject
    , genObjectDeclaresCode
    , writeObjectDeclaresCode
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (lookup)
import qualified Data.Text as T (Text, intercalate)
import qualified Data.Text.Lazy as LT (Text, concat, intercalate)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Text.Shakespeare.Text (lt)
import qualified Text.XML as XML (Element)
import Text.XML.Lens
import qualified Types

parseObject :: Map T.Text Types.Command -> XML.Element -> Maybe Types.Object
parseObject commands elem = do
    name <- elem ^? attr "name"
    constructor <- parseConstructor commands =<< elem ^? el "object" ./ el "constructor"
    destructor <- parseDestructor commands =<< elem ^? el "object" ./ el "destructor"
    return (Types.Object name constructor destructor)

parseConstructor :: Map T.Text Types.Command -> XML.Element -> Maybe Types.ObjectConstructor
parseConstructor commands elem = do
    name <- elem ^? el "constructor" ./ el "name" . text
    type' <- parseConstructorType =<< elem ^? attr "type"
    command <- Map.lookup name commands
    return (Types.ObjectConstructor command type')

parseDestructor :: Map T.Text Types.Command -> XML.Element -> Maybe Types.ObjectDestructor
parseDestructor commands elem = do
    name <- elem ^? el "destructor" ./ el "name" . text
    type' <- parseDestructorType =<< elem ^? attr "type"
    command <- Map.lookup name commands
    return (Types.ObjectDestructor command type')

parseConstructorType :: T.Text -> Maybe Types.ConstructorType
parseConstructorType "Multiple"     = Just Types.ConstructorMultiple
parseConstructorType "SingleReturn" = Just Types.ConstructorSingleReturn
parseConstructorType _              = Nothing

parseDestructorType :: T.Text -> Maybe Types.DestructorType
parseDestructorType "Multiple" = Just Types.DestructorMultiple
parseDestructorType "Single"   = Just Types.DestructorSingle
parseDestructorType _          = Nothing

genObjectDeclaresCode :: [Types.Object] -> LT.Text
genObjectDeclaresCode objects =
    [lt|module GLW.Internal.Objects
    ( #{T.intercalate "(..)\n    , " objectNames}(..)
    ) where

import GLW.Classes

#{LT.concat objectDeclares}
|]
    where
    objectNames = map Types.objectName objects
    objectDeclares = map genObjectDeclare objects

writeObjectDeclaresCode :: [Types.Object] -> IO ()
writeObjectDeclaresCode objects =
    let code = genObjectDeclaresCode objects
        path = "gl-wrapper/GLW/Internal/Objects.hs"
    in LT.writeFile path code

genObjectDeclare :: Types.Object -> LT.Text
genObjectDeclare object =
    [lt|newtype #{objectName} = #{objectName}
    { un#{objectName} :: GL.GLuint
    } deriving (Show, Eq)

#{objectInstanceDeclare}
|]
    where
    objectName = Types.objectName object
    objectInstanceDeclare = genObjectInstanceDeclare object

genObjectInstanceDeclare :: Types.Object -> LT.Text
genObjectInstanceDeclare object =
    [lt|instance GLObject #{objectName} where
    #{createObjectDeclare}
    #{deleteObjectDeclare}
|]
    where
    objectName = Types.objectName object
    createObjectDeclare = genCreateObjectDeclare . Types.objectConstructor $ object
    deleteObjectDeclare = genDeleteObjectDeclare . Types.objectDestructor $ object

genCreateObjectDeclare :: Types.ObjectConstructor -> LT.Text
genCreateObjectDeclare (Types.ObjectConstructor command Types.ConstructorMultiple) =
    [lt|createObjects = mkCreateObjects #{Types.commandName command}|]
genCreateObjectDeclare (Types.ObjectConstructor command Types.ConstructorSingleReturn) =
    [lt|createObject = mkCreateObject #{Types.commandName command}|]

genDeleteObjectDeclare :: Types.ObjectDestructor -> LT.Text
genDeleteObjectDeclare (Types.ObjectDestructor command Types.DestructorMultiple) =
    [lt|deleteObjects = mkDeleteObjects #{Types.commandName command}|]
genDeleteObjectDeclare (Types.ObjectDestructor command Types.DestructorSingle) =
    [lt|deleteObject = mkDeleteObject #{Types.commandName command}|]
