{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Object
    ( parseObject
    , genObjectDeclaresCode
    , writeObjectDeclaresCode
    , writeObjectInstancesCode
    , writeAll
    ) where

import Data.Maybe (catMaybes)
import qualified Data.Text as T (Text, intercalate)
import qualified Data.Text.Lazy as LT (Text, intercalate)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Text.Shakespeare.Text (lt)
import qualified Text.XML as XML (Element)
import Text.XML.Lens
import qualified Types

parseObject :: XML.Element -> Maybe Types.Object
parseObject elem' = do
    oname <- elem' ^? attr "name"
    let discriminator = parseDiscriminator =<< elem' ^? el "object" ./ el "discriminator"
    constructor <- parseConstructor =<< elem' ^? el "object" ./ el "constructor"
    destructor <- parseDestructor =<< elem' ^? el "object" ./ el "destructor"
    return (Types.Object oname constructor destructor discriminator)

parseConstructor :: XML.Element -> Maybe Types.ObjectConstructor
parseConstructor elem' = do
    cname <- elem' ^? el "constructor" ./ el "name" . text
    type' <- parseConstructorType =<< elem' ^? attr "type"
    return (Types.ObjectConstructor cname type')

parseDestructor :: XML.Element -> Maybe Types.ObjectDestructor
parseDestructor elem' = do
    dname <- elem' ^? el "destructor" ./ el "name" . text
    type' <- parseDestructorType =<< elem' ^? attr "type"
    return (Types.ObjectDestructor dname type')

parseDiscriminator :: XML.Element -> Maybe Types.ObjectDiscriminator
parseDiscriminator elem' = do
    dname <- elem' ^? attr "name"
    let members = elem' ^.. el "discriminator" ./ el "member" . text
    return (Types.ObjectDiscriminator dname members)

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
    [lt|{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
module GLW.Internal.ObjectTypes
    ( #{T.intercalate "(..)\n    , " objectNames}(..)
    , #{T.intercalate "(..)\n    , " discriminatorNames}(..)
    , Sing#{T.intercalate "(..)\n    , Sing" discriminatorNames}(..)
    ) where

import Data.Proxy
import Foreign.Storable (Storable(..))
import qualified Graphics.GL as GL

#{LT.intercalate "\n" objectDeclares}|]
    where
    objectNames = map Types.objectName objects
    objectDeclares = map genObjectDeclare objects
    discriminatorNames = map Types.objectDiscriminatorName . catMaybes . map Types.objectDiscriminator $ objects

genObjectInstancesCode :: [Types.Object] -> LT.Text
genObjectInstancesCode objects =
    [lt|{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GLW.Internal.Objects
    ( #{T.intercalate "(..)\n    , " objectNames}(..)
    , #{T.intercalate "(..)\n    , " discriminatorNames}(..)
    , Sing#{T.intercalate "(..)\n    , Sing" discriminatorNames}(..)
    , Object(..)
    ) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (Coercible, coerce)
import Data.Proxy (Proxy(..))
import qualified Foreign (Ptr, allocaArray, peekArray, withArray)
import Foreign.Storable (Storable(..))
import qualified Graphics.GL as GL
import qualified GLW.Commands as GLW
import GLW.Internal.ObjectTypes

mkCreateObjects :: (MonadIO m, Storable a) => (GL.GLsizei -> Foreign.Ptr a -> IO ()) -> Proxy a -> Int -> m [a]
mkCreateObjects f _ n =
    liftIO . Foreign.allocaArray n $ \p -> do
        f (fromIntegral n) p
        Foreign.peekArray n p

mkDeleteObjects :: (MonadIO m, Storable a) => (GL.GLsizei -> Foreign.Ptr a -> IO ()) -> [a] -> m ()
mkDeleteObjects f objs =
    liftIO . Foreign.withArray objs $ \p ->
        f (fromIntegral (length objs)) p

class Object a where
    createObject :: MonadIO m => Proxy a -> m a
    createObject p = head <$> createObjects p 1
    createObjects :: MonadIO m => Proxy a -> Int -> m [a]
    createObjects p n = replicateM n (createObject p)
    deleteObject :: MonadIO m => a -> m ()
    deleteObject a = deleteObjects [a]
    deleteObjects :: MonadIO m => [a] -> m ()
    deleteObjects = mapM_ deleteObject

#{LT.intercalate "\n" objectInstances}|]
    where
    objectNames = map Types.objectName objects
    objectInstances = map genObjectInstanceDeclare objects
    discriminatorNames = map Types.objectDiscriminatorName . catMaybes . map Types.objectDiscriminator $ objects

writeObjectDeclaresCode :: FilePath -> [Types.Object] -> IO ()
writeObjectDeclaresCode outputPath objects =
    let code = genObjectDeclaresCode objects
        path = outputPath ++ "/GLW/Internal/ObjectTypes.hs"
    in LT.writeFile path code

writeObjectInstancesCode :: FilePath -> [Types.Object] -> IO ()
writeObjectInstancesCode outputPath objects =
    let code = genObjectInstancesCode objects
        path = outputPath ++ "/GLW/Internal/Objects.hs"
    in LT.writeFile path code

writeAll :: FilePath -> [Types.Object] -> IO ()
writeAll outputPath objects = do
    writeObjectDeclaresCode outputPath objects
    writeObjectInstancesCode outputPath objects

genObjectDeclare :: Types.Object -> LT.Text
genObjectDeclare object @ (Types.Object objectName _ _ Nothing) =
    [lt|newtype #{objectName} = #{objectName}
    { un#{objectName} :: GL.GLuint
    } deriving (Show, Eq, Storable)
|]

genObjectDeclare object @ (Types.Object objectName _ _ (Just discriminator)) =
    [lt|newtype #{objectName} (a :: #{discriminatorName}) = #{objectName}
    { un#{objectName} :: GL.GLuint
    } deriving (Show, Eq, Storable)

#{objectDiscriminatorDeclare}|]
    where
    discriminatorName = Types.objectDiscriminatorName discriminator
    objectDiscriminatorDeclare = genDiscriminatorDeclare discriminator

genDiscriminatorDeclare :: Types.ObjectDiscriminator -> LT.Text
genDiscriminatorDeclare disc =
    [lt|data #{discriminatorName} =
    #{T.intercalate " |\n    " members}
    deriving (Show, Eq)

class Sing#{discriminatorName} (a :: #{discriminatorName}) where
    sing#{discriminatorName} :: Proxy a -> #{discriminatorName}

#{singInstances}
instance Enum #{discriminatorName} where
    #{toEnumCode}

    #{fromEnumCode}
|]
    where
    discriminatorName = Types.objectDiscriminatorName disc
    members = Types.objectDiscriminatorMembers disc
    singInstances = genSingInstances disc
    toEnumCode = genToEnum disc
    fromEnumCode = genFromEnum disc

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
    gen member = [lt|a == GL.#{member} = #{member}|]
    ow = [lt|otherwise = error "Enum.#{discriminatorName}.toEnum: bad argument"|]

genFromEnum :: Types.ObjectDiscriminator -> LT.Text
genFromEnum disc = LT.intercalate "\n    " $ map gen members
    where
    members = Types.objectDiscriminatorMembers disc
    gen member = [lt|fromEnum #{member} = GL.#{member}|]

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
    [lt|createObjects = mkCreateObjects GLW.#{command}|]
genCreateObjectDeclare (Types.ObjectConstructor command Types.ConstructorSingleReturn) Nothing =
    [lt|createObject _ = GLW.#{command}|]
genCreateObjectDeclare (Types.ObjectConstructor command Types.ConstructorMultiple) (Just discriminatorName) =
    [lt|createObjects = mkCreateObjects (GLW.#{command} (fromIntegral . fromEnum . sing#{discriminatorName} $ (Proxy :: Proxy a)))|]
genCreateObjectDeclare (Types.ObjectConstructor command Types.ConstructorSingleReturn) (Just discriminatorName) =
    [lt|createObject _ = GLW.#{command} (fromIntegral . fromEnum . sing#{discriminatorName} $ (Proxy :: Proxy a))|]

genDeleteObjectDeclare :: Types.ObjectDestructor -> LT.Text
genDeleteObjectDeclare (Types.ObjectDestructor command Types.DestructorMultiple) =
    [lt|deleteObjects = mkDeleteObjects GLW.#{command}|]
genDeleteObjectDeclare (Types.ObjectDestructor command Types.DestructorSingle) =
    [lt|deleteObject = GLW.#{command}|]
