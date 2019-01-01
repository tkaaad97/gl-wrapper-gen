{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Object
    ( parseObject
    , genObjectDeclaresCode
    , writeObjectDeclaresCode
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (lookup)
import Data.Maybe (catMaybes)
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
    [lt|{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GLW.Internal.Objects
    ( #{T.intercalate "(..)\n    , " objectNames}(..)
    , #{T.intercalate "(..)\n    , " discriminatorNames}(..)
    , Object(..)
    ) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (Coercible, coerce)
import Data.Proxy (Proxy(..))
import qualified Foreign (Ptr, free, mallocArray, newArray, peekArray)
import qualified Graphics.GL as GL

mkCreateObject :: (MonadIO m, Coercible GL.GLuint a) => m GL.GLuint -> m a
mkCreateObject = fmap coerce

mkCreateObjects :: (MonadIO m, Coercible GL.GLuint a) => (GL.GLsizei -> Foreign.Ptr GL.GLuint -> m ()) -> Int -> m [a]
mkCreateObjects f n = do
    p <- liftIO $ Foreign.mallocArray n
    f (fromIntegral n) p
    a <- liftIO (Foreign.peekArray n p)
    liftIO $ Foreign.free p
    return (coerce a)

mkDeleteObject :: (MonadIO m, Coercible GL.GLuint a) => (GL.GLuint -> m ()) -> a -> m ()
mkDeleteObject = (. coerce)

mkDeleteObjects :: (MonadIO m, Coercible GL.GLuint a) => (GL.GLsizei -> Foreign.Ptr GL.GLuint -> m ()) -> [a] -> m ()
mkDeleteObjects f objs = do
    p <- liftIO $ Foreign.newArray (coerce objs)
    f (fromIntegral (length objs)) p
    liftIO $ Foreign.free p

class Object a where
    createObject :: MonadIO m => m a
    createObject = head <$> createObjects 1
    createObjects :: MonadIO m => Int -> m [a]
    createObjects n = replicateM n createObject
    deleteObject :: MonadIO m => a -> m ()
    deleteObject a = deleteObjects [a]
    deleteObjects :: MonadIO m => [a] -> m ()
    deleteObjects = mapM_ deleteObject

#{LT.intercalate "\n" objectDeclares}|]
    where
    objectNames = map Types.objectName objects
    objectDeclares = map genObjectDeclare objects
    discriminatorNames = map Types.objectDiscriminatorName . catMaybes . map Types.objectDiscriminator $ objects

writeObjectDeclaresCode :: [Types.Object] -> IO ()
writeObjectDeclaresCode objects =
    let code = genObjectDeclaresCode objects
        path = "gl-wrapper/GLW/Internal/Objects.hs"
    in LT.writeFile path code

genObjectDeclare :: Types.Object -> LT.Text
genObjectDeclare object @ (Types.Object objectName _ _ Nothing) =
    [lt|newtype #{objectName} = #{objectName}
    { un#{objectName} :: GL.GLuint
    } deriving (Show, Eq)

#{objectInstanceDeclare}|]
    where
    objectInstanceDeclare = genObjectInstanceDeclare object

genObjectDeclare object @ (Types.Object objectName _ _ (Just discriminator)) =
    [lt|newtype #{objectName} (a :: #{discriminatorName}) = #{objectName}
    { un#{objectName} :: GL.GLuint
    } deriving (Show, Eq)

#{objectDiscriminatorDeclare}
#{objectInstanceDeclare}|]
    where
    discriminatorName = Types.objectDiscriminatorName discriminator
    objectDiscriminatorDeclare = genDiscriminatorDeclare discriminator
    objectInstanceDeclare = genObjectInstanceDeclare object

genDiscriminatorDeclare :: Types.ObjectDiscriminator -> LT.Text
genDiscriminatorDeclare disc =
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
    ow = [lt|otherwise = error "Enum.#{discriminatorName}.toEnum: bad argument"|]

genFromEnum :: Types.ObjectDiscriminator -> LT.Text
genFromEnum disc = LT.intercalate "\n    " $ map gen members
    where
    discriminatorName = Types.objectDiscriminatorName disc
    members = Types.objectDiscriminatorMembers disc
    gen member = [lt|fromEnum #{member} = fromIntegral GL.#{member}|]

genObjectInstanceDeclare :: Types.Object -> LT.Text
genObjectInstanceDeclare (Types.Object objectName constructor destructor Nothing) =
    [lt|instance Object #{objectName} where
    #{createObjectDeclare}
    #{deleteObjectDeclare}
|]
    where
    createObjectDeclare = genCreateObjectDeclare constructor Nothing
    deleteObjectDeclare = genDeleteObjectDeclare destructor

genObjectInstanceDeclare (Types.Object objectName constructor destructor (Just discriminator)) =
    [lt|instance Sing#{discriminatorName} a => Object (#{objectName} (a :: #{discriminatorName})) where
    #{createObjectDeclare}
    #{deleteObjectDeclare}
|]
    where
    discriminatorName = Types.objectDiscriminatorName discriminator
    createObjectDeclare = genCreateObjectDeclare constructor (Just discriminatorName)
    deleteObjectDeclare = genDeleteObjectDeclare destructor

genCreateObjectDeclare :: Types.ObjectConstructor -> Maybe T.Text -> LT.Text
genCreateObjectDeclare (Types.ObjectConstructor command Types.ConstructorMultiple) Nothing =
    [lt|createObjects = mkCreateObjects GL.#{Types.commandName command}|]
genCreateObjectDeclare (Types.ObjectConstructor command Types.ConstructorSingleReturn) Nothing =
    [lt|createObject = mkCreateObject GL.#{Types.commandName command}|]
genCreateObjectDeclare (Types.ObjectConstructor command Types.ConstructorMultiple) (Just discriminatorName) =
    [lt|createObjects = mkCreateObjects (GL.#{Types.commandName command} (fromIntegral . fromEnum . sing#{discriminatorName} $ (Proxy :: Proxy a)))|]
genCreateObjectDeclare (Types.ObjectConstructor command Types.ConstructorSingleReturn) (Just discriminatorName) =
    [lt|createObject = mkCreateObject (GL.#{Types.commandName command} (fromIntegral . fromEnum . sing#{discriminatorName} $ (Proxy :: Proxy a)))|]

genDeleteObjectDeclare :: Types.ObjectDestructor -> LT.Text
genDeleteObjectDeclare (Types.ObjectDestructor command Types.DestructorMultiple) =
    [lt|deleteObjects = mkDeleteObjects GL.#{Types.commandName command}|]
genDeleteObjectDeclare (Types.ObjectDestructor command Types.DestructorSingle) =
    [lt|deleteObject = mkDeleteObject GL.#{Types.commandName command}|]
