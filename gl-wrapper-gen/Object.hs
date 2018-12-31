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
    let discriminator = parseDiscriminator =<< elem ^? el "object" ./ el "discriminator"
    constructor <- parseConstructor commands =<< elem ^? el "object" ./ el "constructor"
    destructor <- parseDestructor commands =<< elem ^? el "object" ./ el "destructor"
    return (Types.Object name constructor destructor discriminator)

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

parseDiscriminator :: XML.Element -> Maybe Types.ObjectDiscriminator
parseDiscriminator elem = do
    name <- elem ^? attr "name"
    let members = elem ^.. el "discriminator" ./ el "member" . text
    return (Types.ObjectDiscriminator name members)

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
    [lt|{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module GLW.Internal.Objects
    ( #{T.intercalate "(..)\n    , " objectNames}(..)
    ) where

import Data.Proxy (Proxy(..))

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

#{objectDiscriminatorDeclare}#{objectInstanceDeclare}
|]
    where
    objectName = Types.objectName object
    objectDiscriminatorDeclare = genDiscriminatorDeclare . Types.objectDiscriminator $ object
    objectInstanceDeclare = genObjectInstanceDeclare object

genDiscriminatorDeclare :: Maybe Types.ObjectDiscriminator -> LT.Text
genDiscriminatorDeclare Nothing = ""
genDiscriminatorDeclare (Just disc) =
    [lt|data #{discriminatorName} =
    #{T.intercalate " |\n    " members}
    deriving (Show, Eq)

class Sing#{discriminatorName} (a :: #{discriminatorName}) where
    sing#{discriminatorName} :: Proxy a -> #{discriminatorName}

#{singInstances}
instance Enum #{discriminatorName} where
    #{toEnum}

    #{fromEnum}

|]
    where
    discriminatorName = Types.objectDiscriminatorName disc
    members = Types.objectDiscriminatorMembers disc
    singInstances = genSingInstances disc
    toEnum = genToEnum disc
    fromEnum = genFromEnum disc

genSingInstances :: Types.ObjectDiscriminator -> LT.Text
genSingInstances disc = LT.intercalate "\n" singInstances
    where
    discriminatorName = Types.objectDiscriminatorName disc
    members = Types.objectDiscriminatorMembers disc
    singInstances = map gen members
    gen member =
        [lt|instance Sing#{discriminatorName} '#{member} where
    sing#{discriminatorName} _ = #{member}
|]

genToEnum :: Types.ObjectDiscriminator -> LT.Text
genToEnum disc = "toEnum a | " `mappend` LT.intercalate "\n        | " (map gen members ++ [ow])
    where
    discriminatorName = Types.objectDiscriminatorName disc
    members = Types.objectDiscriminatorMembers disc
    gen member = [lt|a == fromIntegral GL.#{member} = #{member}|]
    ow = [lt|_ = error "Enum.#{discriminatorName}.toEnum: bad argument"|]

genFromEnum :: Types.ObjectDiscriminator -> LT.Text
genFromEnum disc = LT.intercalate "\n    " $ map gen members
    where
    discriminatorName = Types.objectDiscriminatorName disc
    members = Types.objectDiscriminatorMembers disc
    gen member = [lt|fromEnum #{member} = fromIntegral GL.#{member}|]

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
