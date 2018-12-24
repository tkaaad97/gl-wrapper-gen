{-# LANGUAGE OverloadedStrings #-}
module Types
    ( Group(..)
    , Command(..)
    , Param(..)
    , Type(..)
    , TypeInfo(..)
    , PrimType(..)
    , printPrimType
    ) where

import Data.Text (Text)
import qualified Data.Text as Text (concat, pack)

data Group = Group
    { groupName    :: !Text
    , groupMembers :: ![Text]
    } deriving (Show, Eq)

data Command = Command
    { commandName           :: !Text
    , commandParams         :: ![Param]
    , commandReturnTypeInfo :: !TypeInfo
    } deriving (Show, Eq)

data Param = Param
    { paramName     :: !Text
    , paramTypeInfo :: !TypeInfo
    } deriving (Show, Eq)

data TypeInfo = TypeInfo
    { typeInfoType      :: !Type
    , typeInfoEnumGroup :: !(Maybe Text)
    , typeInfoLength    :: !(Maybe Text)
    } deriving (Show, Eq, Read)

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
