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

import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Text (Text)
import qualified Graphics.GL.Types as GL
import System.IO.Unsafe (unsafePerformIO)

data GLLog =
    GLLogCommand !Text ![GLLogValue] |
    GLLogCommandResult !Text !GLLogValue
    deriving (Show, Eq)

#{declareGLLogValue}

maxLogCount :: Int
maxLogCount = 150

logDropCount :: Int
logDropCount = 50

glLogsRef :: IORef (Int, [GLLog])
glLogsRef = unsafePerformIO . newIORef $ (0, [])
{-# NOINLINE glLogsRef #-}

readGLLogs :: IO [GLLog]
readGLLogs = snd <$> readIORef glLogsRef

logGLCommand :: Text -> [GLLogValue] -> IO ()
logGLCommand command params = writeGLLog $ GLLogCommand command params

logGLCommandResult :: Text -> GLLogValue -> IO ()
logGLCommandResult command result = writeGLLog $ GLLogCommandResult command result

writeGLLog :: GLLog -> IO ()
writeGLLog log' = atomicModifyIORef' glLogsRef f
    where
    f (c, logs)
        | c + 1 >= maxLogCount =
            let remainder = c + 1 - logDropCount
            in ((remainder, take remainder (log' : logs)), ())
        | otherwise =
            ((c + 1, log' : logs), ())

flushGLLogs :: IO ()
flushGLLogs = writeIORef glLogsRef (0, [])
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
    LogV'Ptr |
    LogV'Void
    deriving (Show, Eq)
|]

    conltructorsGLLogValue = LT.intercalate " |\n    " (map genGLLogValueConstructor glTypeNames)

    genGLLogValueConstructor :: LT.Text -> LT.Text
    genGLLogValueConstructor a =
        [lt|LogV'#{a} !GL.#{a}|]
