{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Test
    ( genTestModule
    , writeTestModule
    ) where

import qualified Data.Text.Lazy as LT (Text, intercalate)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Text.Shakespeare.Text (lt)

writeTestModule :: FilePath -> IO ()
writeTestModule outputPath =
    let path = outputPath <> "/GLW/Test.hs"
    in LT.writeFile path genTestModule

genTestModule :: LT.Text
genTestModule =
    [lt|module GLW.Test
    ( GLLog(..)
    , GLLogValue(..)
    , logGLCommand
    , logGLCommandResult
    , readGLLogs
    , writeGLLog
    , flushGLLogs
    ) where

import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import qualified Graphics.GL.Types as GL

data GLLog =
    GLLogCommand ![GLLogValue] |
    GLLogCommandResult !GLLogValue
    deriving (Show, Eq)

#{declareGLLogValue}

glLogsRef :: IO (IORef [GLLog])
glLogsRef = unsafePerformIO . newIORef $ []
{-# NOINLINE glLogsRef #-}

readGLLogs :: IO [GLLog]
readGLLogs = readIORef glLogsRef

logGLCommand :: [GLLogValue] -> IO ()
logGLCommand = writeGLLog . GLLogCommand

logGLCommandResult :: GLLogValue -> IO ()
logGLCommandResult = writeGLLog . GLLogCommandResult

writeGLLog :: GLLog -> IO ()
writeGLLog a = atomicModifyIORef' glLogsRef (\logs -> (a : logs, ()))

flushGLLogs :: IO ()
flushGLLogs = writeIORef glLogsRef []
|]
    where
    glTypeNames =
        [ "GLDEBUGPROC"
        , "GLDEBUGPROCAMD"
        , "GLDEBUGPROCARB"
        , "GLDEBUGPROCKHR"
        , "GLbitfield"
        , "GLboolean"
        , "GLbyte"
        , "GLchar"
        , "GLcharARB"
        , "GLclampd"
        , "GLclampf"
        , "GLclampx"
        , "GLdouble"
        , "GLeglImageOES"
        , "GLenum"
        , "GLfixed"
        , "GLfloat"
        , "GLhalf"
        , "GLhalfARB"
        , "GLhalfNV"
        , "GLhandleARB"
        , "GLint"
        , "GLint64"
        , "GLint64EXT"
        , "GLintptr"
        , "GLintptrARB"
        , "GLshort"
        , "GLsizei"
        , "GLsizeiptr"
        , "GLsizeiptrARB"
        , "GLsync"
        , "GLubyte"
        , "GLuint"
        , "GLuint64"
        , "GLuint64EXT"
        , "GLushort"
        , "GLvdpauSurfaceNV"
        ]

    declareGLLogValue =
        [lt|data GLLogValue =
    #{conltructorsGLLogValue} |
    LogV'Void
    deriving (Show, Eq)
|]

    conltructorsGLLogValue = LT.intercalate " |\n    " (map genGLLogValueConstructor glTypeNames)

    genGLLogValueConstructor :: LT.Text -> LT.Text
    genGLLogValueConstructor a =
        [lt|LogV'#{a} !GL.#{a}|]
