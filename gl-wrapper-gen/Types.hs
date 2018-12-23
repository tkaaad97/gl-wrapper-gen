module Types
    ( Group(..)
    , Command(..)
    , Param(..)
    , Type(..)
    , TypeInfo(..)
    , PrimType(..)
    ) where

import Data.Text (Text)

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
