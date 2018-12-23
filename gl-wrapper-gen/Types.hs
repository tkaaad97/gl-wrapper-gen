module Types
    ( Group(..)
    , Command(..)
    , Param(..)
    , ParamType(..)
    , PrimType(..)
    ) where

import Data.Text (Text)

data Group = Group
    { groupName    :: !Text
    , groupMembers :: ![Text]
    } deriving (Show, Eq)

data Command = Command
    { commandName       :: !Text
    , commandParams     :: ![Param]
    , commandReturnType :: !ParamType
    } deriving (Show, Eq)

data Param = Param
    { paramName :: !Text
    , paramType :: !ParamType
    } deriving (Show, Eq)

data ParamType =
    ParamTypePrim PrimType |
    ParamTypePtr ParamType
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
