{-# LANGUAGE OverloadedStrings #-}
module Types
    ( Command(..)
    , ConstructorType(..)
    , DestructorType(..)
    , Group(..)
    , Newtype(..)
    , Object(..)
    , ObjectConstructor(..)
    , ObjectDestructor(..)
    , ObjectDiscriminator(..)
    , Param(..)
    , PrimType(..)
    , ReturnTypeInfo(..)
    , Type(..)
    , TypeInfo(..)
    , parsePrimType
    , printPrimType
    ) where

import Data.Text (Text)
import qualified Data.Text as Text (concat, pack, unpack)
import Text.Read (readMaybe)

data Group = Group
    { groupName       :: !Text
    , groupMembers    :: ![Text]
    , groupMemberType :: !PrimType
    } deriving (Show, Eq)

data Command = Command
    { commandName           :: !Text
    , commandParams         :: ![Param]
    , commandReturnTypeInfo :: !ReturnTypeInfo
    } deriving (Show, Eq)

data Param = Param
    { paramName     :: !Text
    , paramTypeInfo :: !TypeInfo
    } deriving (Show, Eq)

data ReturnTypeInfo = ReturnTypeInfo
    { returnTypeInfoTypeInfo   :: !TypeInfo
    , returnTypeInfoValidation :: !(Maybe Text)
    } deriving (Show, Eq)

data TypeInfo = TypeInfo
    { typeInfoType      :: !Type
    , typeInfoEnumGroup :: !(Maybe Text)
    , typeInfoObject    :: !(Maybe Object)
    , typeInfoNewtype   :: !(Maybe Newtype)
    , typeInfoLength    :: !(Maybe Text)
    } deriving (Show, Eq)

data Type =
    TypePrim PrimType |
    TypePtr Type
    deriving (Show, Eq, Read)

data PrimType =
    GLDEBUGPROC |
    GLDEBUGPROCAMD |
    GLDEBUGPROCARB |
    GLDEBUGPROCKHR |
    GLbitfield |
    GLboolean |
    GLbyte |
    GLchar |
    GLcharARB |
    GLclampd |
    GLclampf |
    GLclampx |
    GLdouble |
    GLeglImageOES |
    GLenum |
    GLfixed |
    GLfloat |
    GLhalf |
    GLhalfARB |
    GLhalfNV |
    GLhandleARB |
    GLint |
    GLint64 |
    GLint64EXT |
    GLintptr |
    GLintptrARB |
    GLshort |
    GLsizei |
    GLsizeiptr |
    GLsizeiptrARB |
    GLsync |
    GLubyte |
    GLuint |
    GLuint64 |
    GLuint64EXT |
    GLushort |
    GLvdpauSurfaceNV |
    GLvoid |
    Void
    deriving (Show, Eq, Read)

printPrimType :: Text -> PrimType -> Text
printPrimType _ Void   = "()"
printPrimType prefix a = Text.concat [prefix, Text.pack . show $ a]

parsePrimType :: Maybe Text -> Maybe Types.PrimType
parsePrimType (Just p) = readMaybe . Text.unpack $ p
parsePrimType Nothing  = Just Types.Void

data Object = Object
    { objectName          :: !Text
    , objectConstructor   :: !ObjectConstructor
    , objectDestructor    :: !ObjectDestructor
    , objectDiscriminator :: !(Maybe ObjectDiscriminator)
    } deriving (Show, Eq)

data ObjectConstructor = ObjectConstructor
    { objectConstructorCommand :: !Text
    , objectConstructorType    :: !ConstructorType
    } deriving (Show, Eq)

data ObjectDestructor = ObjectDestructor
    { objectDestructorCommand :: !Text
    , objectDestructorType    :: !DestructorType
    } deriving (Show, Eq)

data ObjectDiscriminator = ObjectDiscriminator
    { objectDiscriminatorName    :: !Text
    , objectDiscriminatorMembers :: ![Text]
    } deriving (Show, Eq)

data ConstructorType =
    ConstructorMultiple |
    ConstructorSingleReturn
    deriving (Show, Eq)

data DestructorType =
    DestructorMultiple |
    DestructorSingle
    deriving (Show, Eq)

data Newtype = Newtype
    { newtypeName :: !Text
    , newtypeType :: !PrimType
    } deriving (Show, Eq)
