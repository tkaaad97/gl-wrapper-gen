module Types
    ( Group(..)
    , Command(..)
    , Param(..)
    , ParamType(..)
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
    ParamTypePtr PrimType |
    ParamTypeConstPtr PrimType
    deriving (Show, Eq)

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
    Other !Text
    deriving (Show, Eq)
