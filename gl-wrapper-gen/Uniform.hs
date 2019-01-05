{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Uniform
    ( genUniformCode
    , writeUniformCode
    ) where

import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as LT (Text, intercalate, pack)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Text.Shakespeare.Text (lt)

writeUniformCode :: IO ()
writeUniformCode =
    let code = genUniformCode
        path = "gl-wrapper/GLW/Uniforms.hs"
    in LT.writeFile path code

genUniformCode :: LT.Text
genUniformCode =
    [lt|{-# LANGUAGE FlexibleInstances    #-}
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

#{LT.intercalate "\n" uniformComponentInstances}
#{LT.intercalate "\n" uniformInstanceVecs}
#{LT.intercalate "\n" uniformInstanceMats}
|]
    where
    uniformComponentInstances = map (uncurry genUniformComponentInstance)
        [("GL.GLfloat", "f"), ("GL.GLint", "i"), ("GL.GLuint", "ui"), ("GL.GLdouble", "d")]
    uniformInstanceVecs = map genUniformInstanceVec [1..4]
    uniformInstanceMats =
        [genUniformInstanceMat row col "GL.GLfloat" "f" | row <- [2..4], col <- [2..4]] ++
        [genUniformInstanceMat row col "GL.GLdouble" "d" | row <- [2..4], col <- [2..4]]

genUniformComponentInstance :: T.Text -> T.Text -> LT.Text
genUniformComponentInstance type' postfix =
    [lt|instance UniformComponent #{type'} where
    uniform1 = GL.glUniform1#{postfix}
    uniform2 = GL.glUniform2#{postfix}
    uniform3 = GL.glUniform3#{postfix}
    uniform4 = GL.glUniform4#{postfix}
    uniform1v = GL.glUniform1#{postfix}v
    uniform2v = GL.glUniform2#{postfix}v
    uniform3v = GL.glUniform3#{postfix}v
    uniform4v = GL.glUniform4#{postfix}v
    programUniform1 = GL.glProgramUniform1#{postfix} . coerce
    programUniform2 = GL.glProgramUniform2#{postfix} . coerce
    programUniform3 = GL.glProgramUniform3#{postfix} . coerce
    programUniform4 = GL.glProgramUniform4#{postfix} . coerce
    programUniform1v = GL.glProgramUniform1#{postfix}v . coerce
    programUniform2v = GL.glProgramUniform2#{postfix}v . coerce
    programUniform3v = GL.glProgramUniform3#{postfix}v . coerce
    programUniform4v = GL.glProgramUniform4#{postfix}v . coerce
    getUniformv = GL.glGetUniform#{postfix}v . coerce
    getnUniformv = GL.glGetnUniform#{postfix}v . coerce
|]

genUniformInstanceVec :: Int -> LT.Text
genUniformInstanceVec len =
    [lt|instance UniformComponent a => Uniform (V#{len} a) where
    uniform loc (V#{len} #{vargs}) = uniform#{len} loc #{vargs}
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        uniform#{len}v loc (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)
    programUniform program loc (V#{len} #{vargs}) = programUniform#{len} program loc #{vargs}
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        programUniform#{len}v program loc (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)
|]
    where
    vargs = LT.intercalate " " ["a" <> LT.pack (show x) |x <- [0..(len - 1)]]

genUniformInstanceMat :: Int -> Int -> T.Text -> T.Text -> LT.Text
genUniformInstanceMat row col type' postfix =
    [lt|instance Uniform (V#{row} (V#{col} #{type'})) where
    uniform loc a = liftIO . Foreign.with a $ \p ->
        #{ufunc}v loc 1 GL.GL_TRUE (coerce p)
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        #{ufunc}v loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
    programUniform program loc a = liftIO . Foreign.with a $ \p ->
        #{pufunc}v (coerce program) loc 1 GL.GL_TRUE (coerce p)
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        #{pufunc}v (coerce program) loc (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
|]
    where
    ufunc = genUniformMatrixFunctionName "GL.glUniformMatrix" row col postfix
    pufunc = genUniformMatrixFunctionName "GL.glProgramUniformMatrix" row col postfix

genUniformMatrixFunctionName :: T.Text -> Int -> Int -> T.Text -> LT.Text
genUniformMatrixFunctionName base row col post
    | row == col = [lt|#{base}#{row}#{post}|]
    | otherwise = [lt|#{base}#{row}x#{col}#{post}|]
