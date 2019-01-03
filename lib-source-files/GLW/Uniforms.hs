{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GLW.Uniforms
    ( Uniform(..)
    , UniformComponent(..)
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector (length, unsafeWith)
import Foreign (Ptr, Storable(..), with)
import GLW.Internal.Objects (Program(..))
import qualified Graphics.GL as GL
import Linear (V1(..), V2(..), V3(..), V4(..))

class Storable a => UniformComponent a where
    uniform1 :: MonadIO m => GL.GLint -> a -> m ()
    uniform2 :: MonadIO m => GL.GLint -> a -> a -> m ()
    uniform3 :: MonadIO m => GL.GLint -> a -> a -> a -> m ()
    uniform4 :: MonadIO m => GL.GLint -> a -> a -> a -> a -> m ()
    uniform1v :: MonadIO m => GL.GLint -> GL.GLsizei -> Ptr a -> m ()
    uniform2v :: MonadIO m => GL.GLint -> GL.GLsizei -> Ptr a -> m ()
    uniform3v :: MonadIO m => GL.GLint -> GL.GLsizei -> Ptr a -> m ()
    uniform4v :: MonadIO m => GL.GLint -> GL.GLsizei -> Ptr a -> m ()

    programUniform1 :: MonadIO m => Program -> GL.GLint -> a -> m ()
    programUniform2 :: MonadIO m => Program -> GL.GLint -> a -> a -> m ()
    programUniform3 :: MonadIO m => Program -> GL.GLint -> a -> a -> a -> m ()
    programUniform4 :: MonadIO m => Program -> GL.GLint -> a -> a -> a -> a -> m ()
    programUniform1v :: MonadIO m => Program -> GL.GLint -> GL.GLsizei -> Ptr a -> m ()
    programUniform2v :: MonadIO m => Program -> GL.GLint -> GL.GLsizei -> Ptr a -> m ()
    programUniform3v :: MonadIO m => Program -> GL.GLint -> GL.GLsizei -> Ptr a -> m ()
    programUniform4v :: MonadIO m => Program -> GL.GLint -> GL.GLsizei -> Ptr a -> m ()

    getUniformv :: Program -> GL.GLint -> Ptr a -> IO ()
    getnUniformv :: Program -> GL.GLint -> GL.GLsizei -> Ptr a -> IO ()

class Storable a => Uniform a where
    uniform :: MonadIO m => GL.GLint -> a -> m ()
    uniformv :: MonadIO m => GL.GLint -> Vector a -> m ()
    programUniform :: MonadIO m => Program -> GL.GLint -> a -> m ()
    programUniformv :: MonadIO m => Program -> GL.GLint -> Vector a -> m ()

instance UniformComponent GL.GLfloat where
    uniform1 = GL.glUniform1f
    uniform2 = GL.glUniform2f
    uniform3 = GL.glUniform3f
    uniform4 = GL.glUniform4f
    uniform1v = GL.glUniform1fv
    uniform2v = GL.glUniform2fv
    uniform3v = GL.glUniform3fv
    uniform4v = GL.glUniform4fv
    programUniform1 = GL.glProgramUniform1f . coerce
    programUniform2 = GL.glProgramUniform2f . coerce
    programUniform3 = GL.glProgramUniform3f . coerce
    programUniform4 = GL.glProgramUniform4f . coerce
    programUniform1v = GL.glProgramUniform1fv . coerce
    programUniform2v = GL.glProgramUniform2fv . coerce
    programUniform3v = GL.glProgramUniform3fv . coerce
    programUniform4v = GL.glProgramUniform4fv . coerce
    getUniformv = GL.glGetUniformfv . coerce
    getnUniformv = GL.glGetnUniformfv . coerce

instance UniformComponent GL.GLint where
    uniform1 = GL.glUniform1i
    uniform2 = GL.glUniform2i
    uniform3 = GL.glUniform3i
    uniform4 = GL.glUniform4i
    uniform1v = GL.glUniform1iv
    uniform2v = GL.glUniform2iv
    uniform3v = GL.glUniform3iv
    uniform4v = GL.glUniform4iv
    programUniform1 = GL.glProgramUniform1i . coerce
    programUniform2 = GL.glProgramUniform2i . coerce
    programUniform3 = GL.glProgramUniform3i . coerce
    programUniform4 = GL.glProgramUniform4i . coerce
    programUniform1v = GL.glProgramUniform1iv . coerce
    programUniform2v = GL.glProgramUniform2iv . coerce
    programUniform3v = GL.glProgramUniform3iv . coerce
    programUniform4v = GL.glProgramUniform4iv . coerce
    getUniformv = GL.glGetUniformiv . coerce
    getnUniformv = GL.glGetnUniformiv . coerce

instance UniformComponent GL.GLuint where
    uniform1 = GL.glUniform1ui
    uniform2 = GL.glUniform2ui
    uniform3 = GL.glUniform3ui
    uniform4 = GL.glUniform4ui
    uniform1v = GL.glUniform1uiv
    uniform2v = GL.glUniform2uiv
    uniform3v = GL.glUniform3uiv
    uniform4v = GL.glUniform4uiv
    programUniform1 = GL.glProgramUniform1ui . coerce
    programUniform2 = GL.glProgramUniform2ui . coerce
    programUniform3 = GL.glProgramUniform3ui . coerce
    programUniform4 = GL.glProgramUniform4ui . coerce
    programUniform1v = GL.glProgramUniform1uiv . coerce
    programUniform2v = GL.glProgramUniform2uiv . coerce
    programUniform3v = GL.glProgramUniform3uiv . coerce
    programUniform4v = GL.glProgramUniform4uiv . coerce
    getUniformv = GL.glGetUniformuiv . coerce
    getnUniformv = GL.glGetnUniformuiv . coerce

instance UniformComponent GL.GLdouble where
    uniform1 = GL.glUniform1d
    uniform2 = GL.glUniform2d
    uniform3 = GL.glUniform3d
    uniform4 = GL.glUniform4d
    uniform1v = GL.glUniform1dv
    uniform2v = GL.glUniform2dv
    uniform3v = GL.glUniform3dv
    uniform4v = GL.glUniform4dv
    programUniform1 = GL.glProgramUniform1d . coerce
    programUniform2 = GL.glProgramUniform2d . coerce
    programUniform3 = GL.glProgramUniform3d . coerce
    programUniform4 = GL.glProgramUniform4d . coerce
    programUniform1v = GL.glProgramUniform1dv . coerce
    programUniform2v = GL.glProgramUniform2dv . coerce
    programUniform3v = GL.glProgramUniform3dv . coerce
    programUniform4v = GL.glProgramUniform4dv . coerce
    getUniformv = GL.glGetUniformdv . coerce
    getnUniformv = GL.glGetnUniformdv . coerce

instance UniformComponent a => Uniform (V1 a) where
    uniform loc (V1 a) = uniform1 loc a
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        uniform1v loc (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)
    programUniform program loc (V1 a) = programUniform1 program loc a
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        programUniform1v program loc (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)

instance UniformComponent a => Uniform (V2 a) where
    uniform loc (V2 a0 a1) = uniform2 loc a0 a1
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        uniform2v loc (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)
    programUniform program loc (V2 a0 a1) = programUniform2 program loc a0 a1
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        programUniform2v program loc (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)

instance UniformComponent a => Uniform (V3 a) where
    uniform loc (V3 a0 a1 a2) = uniform3 loc a0 a1 a2
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        uniform3v loc (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)
    programUniform program loc (V3 a0 a1 a2) = programUniform3 program loc a0 a1 a2
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        programUniform3v program loc (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)

instance UniformComponent a => Uniform (V4 a) where
    uniform loc (V4 a0 a1 a2 a3) = uniform4 loc a0 a1 a2 a3
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        uniform4v loc (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)
    programUniform program loc (V4 a0 a1 a2 a3) = programUniform4 program loc a0 a1 a2 a3
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        programUniform4v program loc (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)

instance Uniform (V2 (V2 GL.GLfloat)) where
    uniform loc a = liftIO . Foreign.with a $ \p ->
        GL.glUniformMatrix2fv loc 1 GL.GL_TRUE (coerce p)
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glUniformMatrix2fv loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
    programUniform program loc a = liftIO . Foreign.with a $ \p ->
        GL.glProgramUniformMatrix2fv (coerce program) loc 1 GL.GL_TRUE (coerce p)
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glProgramUniformMatrix2fv (coerce program) loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)

instance Uniform (V2 (V3 GL.GLfloat)) where
    uniform loc a = liftIO . Foreign.with a $ \p ->
        GL.glUniformMatrix2x3fv loc 1 GL.GL_TRUE (coerce p)
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glUniformMatrix2x3fv loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
    programUniform program loc a = liftIO . Foreign.with a $ \p ->
        GL.glProgramUniformMatrix2x3fv (coerce program) loc 1 GL.GL_TRUE (coerce p)
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glProgramUniformMatrix2x3fv (coerce program) loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)

instance Uniform (V2 (V4 GL.GLfloat)) where
    uniform loc a = liftIO . Foreign.with a $ \p ->
        GL.glUniformMatrix2x4fv loc 1 GL.GL_TRUE (coerce p)
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glUniformMatrix2x4fv loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
    programUniform program loc a = liftIO . Foreign.with a $ \p ->
        GL.glProgramUniformMatrix2x4fv (coerce program) loc 1 GL.GL_TRUE (coerce p)
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glProgramUniformMatrix2x4fv (coerce program) loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)

instance Uniform (V3 (V3 GL.GLfloat)) where
    uniform loc a = liftIO . Foreign.with a $ \p ->
        GL.glUniformMatrix3fv loc 1 GL.GL_TRUE (coerce p)
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glUniformMatrix3fv loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
    programUniform program loc a = liftIO . Foreign.with a $ \p ->
        GL.glProgramUniformMatrix3fv (coerce program) loc 1 GL.GL_TRUE (coerce p)
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glProgramUniformMatrix3fv (coerce program) loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)

instance Uniform (V3 (V2 GL.GLfloat)) where
    uniform loc a = liftIO . Foreign.with a $ \p ->
        GL.glUniformMatrix3x2fv loc 1 GL.GL_TRUE (coerce p)
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glUniformMatrix3x2fv loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
    programUniform program loc a = liftIO . Foreign.with a $ \p ->
        GL.glProgramUniformMatrix3x2fv (coerce program) loc 1 GL.GL_TRUE (coerce p)
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glProgramUniformMatrix3x2fv (coerce program) loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)

instance Uniform (V3 (V4 GL.GLfloat)) where
    uniform loc a = liftIO . Foreign.with a $ \p ->
        GL.glUniformMatrix3x4fv loc 1 GL.GL_TRUE (coerce p)
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glUniformMatrix3x4fv loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
    programUniform program loc a = liftIO . Foreign.with a $ \p ->
        GL.glProgramUniformMatrix3x4fv (coerce program) loc 1 GL.GL_TRUE (coerce p)
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glProgramUniformMatrix3x4fv (coerce program) loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)

instance Uniform (V4 (V4 GL.GLfloat)) where
    uniform loc a = liftIO . Foreign.with a $ \p ->
        GL.glUniformMatrix4fv loc 1 GL.GL_TRUE (coerce p)
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glUniformMatrix4fv loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
    programUniform program loc a = liftIO . Foreign.with a $ \p ->
        GL.glProgramUniformMatrix4fv (coerce program) loc 1 GL.GL_TRUE (coerce p)
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glProgramUniformMatrix4fv (coerce program) loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)

instance Uniform (V4 (V2 GL.GLfloat)) where
    uniform loc a = liftIO . Foreign.with a $ \p ->
        GL.glUniformMatrix4x2fv loc 1 GL.GL_TRUE (coerce p)
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glUniformMatrix4x2fv loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
    programUniform program loc a = liftIO . Foreign.with a $ \p ->
        GL.glProgramUniformMatrix4x2fv (coerce program) loc 1 GL.GL_TRUE (coerce p)
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glProgramUniformMatrix4x2fv (coerce program) loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)

instance Uniform (V4 (V3 GL.GLfloat)) where
    uniform loc a = liftIO . Foreign.with a $ \p ->
        GL.glUniformMatrix4x3fv loc 1 GL.GL_TRUE (coerce p)
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glUniformMatrix4x3fv loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
    programUniform program loc a = liftIO . Foreign.with a $ \p ->
        GL.glProgramUniformMatrix4x3fv (coerce program) loc 1 GL.GL_TRUE (coerce p)
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        GL.glProgramUniformMatrix4x3fv (coerce program) loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
