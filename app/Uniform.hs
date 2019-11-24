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

writeUniformCode :: FilePath -> IO ()
writeUniformCode outputPath =
    let code = genUniformCode
        path = outputPath ++ "/GLW/Uniforms.hs"
    in LT.writeFile path code

genUniformCode :: LT.Text
genUniformCode =
    [lt|{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GLW.Uniforms
    ( Uniform(..)
    , UniformComponent(..)
    , getUniformLocation
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString, useAsCString)
import Data.Coerce (coerce)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector (length, unsafeWith)
import Foreign (Ptr, Storable(..), with)
import GLW.Internal.Objects (Program(..))
import GLW.Types (UniformLocation(..))
import qualified Graphics.GL as GL
import Linear (V1(..), V2(..), V3(..), V4(..))

getUniformLocation :: Program -> ByteString -> IO (Maybe UniformLocation)
getUniformLocation p uname = do
    loc <- useAsCString uname $ \cs -> GL.glGetUniformLocation (coerce p) cs
    if loc < 0
        then return Nothing
        else return (Just (UniformLocation loc))

class Storable a => UniformComponent a where
    uniform1 :: MonadIO m => UniformLocation -> a -> m ()
    uniform2 :: MonadIO m => UniformLocation -> a -> a -> m ()
    uniform3 :: MonadIO m => UniformLocation -> a -> a -> a -> m ()
    uniform4 :: MonadIO m => UniformLocation -> a -> a -> a -> a -> m ()
    uniform1v :: MonadIO m => UniformLocation -> GL.GLsizei -> Ptr a -> m ()
    uniform2v :: MonadIO m => UniformLocation -> GL.GLsizei -> Ptr a -> m ()
    uniform3v :: MonadIO m => UniformLocation -> GL.GLsizei -> Ptr a -> m ()
    uniform4v :: MonadIO m => UniformLocation -> GL.GLsizei -> Ptr a -> m ()

    programUniform1 :: MonadIO m => Program -> UniformLocation -> a -> m ()
    programUniform2 :: MonadIO m => Program -> UniformLocation -> a -> a -> m ()
    programUniform3 :: MonadIO m => Program -> UniformLocation -> a -> a -> a -> m ()
    programUniform4 :: MonadIO m => Program -> UniformLocation -> a -> a -> a -> a -> m ()
    programUniform1v :: MonadIO m => Program -> UniformLocation -> GL.GLsizei -> Ptr a -> m ()
    programUniform2v :: MonadIO m => Program -> UniformLocation -> GL.GLsizei -> Ptr a -> m ()
    programUniform3v :: MonadIO m => Program -> UniformLocation -> GL.GLsizei -> Ptr a -> m ()
    programUniform4v :: MonadIO m => Program -> UniformLocation -> GL.GLsizei -> Ptr a -> m ()

    getUniformv :: Program -> UniformLocation -> Ptr a -> IO ()
    getnUniformv :: Program -> UniformLocation -> GL.GLsizei -> Ptr a -> IO ()

class Storable a => Uniform a where
    uniform :: MonadIO m => UniformLocation -> a -> m ()
    uniformv :: MonadIO m => UniformLocation -> Vector a -> m ()
    programUniform :: MonadIO m => Program -> UniformLocation -> a -> m ()
    programUniformv :: MonadIO m => Program -> UniformLocation -> Vector a -> m ()

#{LT.intercalate "\n" uniformComponentInstances}
#{LT.intercalate "\n" uniformInstancePrims}
#{LT.intercalate "\n" uniformInstanceVecs}
#{LT.intercalate "\n" uniformInstanceMats}
|]
    where
    uniformComponentInstances = map (uncurry genUniformComponentInstance)
        [("GL.GLfloat", "f"), ("GL.GLint", "i"), ("GL.GLuint", "ui"), ("GL.GLdouble", "d")]
    uniformInstancePrims = map genUniformInstancePrim
        ["GL.GLfloat", "GL.GLint", "GL.GLuint", "GL.GLdouble"]
    uniformInstanceVecs = map genUniformInstanceVec [1..4]
    uniformInstanceMats =
        [genUniformInstanceMat row col "GL.GLfloat" "f" | row <- [2..4], col <- [2..4]] ++
        [genUniformInstanceMat row col "GL.GLdouble" "d" | row <- [2..4], col <- [2..4]]

genUniformComponentInstance :: T.Text -> T.Text -> LT.Text
genUniformComponentInstance type' postfix =
    [lt|instance UniformComponent #{type'} where
    uniform1 = GL.glUniform1#{postfix} . coerce
    uniform2 = GL.glUniform2#{postfix} . coerce
    uniform3 = GL.glUniform3#{postfix} . coerce
    uniform4 = GL.glUniform4#{postfix} . coerce
    uniform1v = GL.glUniform1#{postfix}v . coerce
    uniform2v = GL.glUniform2#{postfix}v . coerce
    uniform3v = GL.glUniform3#{postfix}v . coerce
    uniform4v = GL.glUniform4#{postfix}v . coerce
    programUniform1 p = GL.glProgramUniform1#{postfix} (coerce p) . coerce
    programUniform2 p = GL.glProgramUniform2#{postfix} (coerce p) . coerce
    programUniform3 p = GL.glProgramUniform3#{postfix} (coerce p) . coerce
    programUniform4 p = GL.glProgramUniform4#{postfix} (coerce p) . coerce
    programUniform1v p = GL.glProgramUniform1#{postfix}v (coerce p) . coerce
    programUniform2v p = GL.glProgramUniform2#{postfix}v (coerce p) . coerce
    programUniform3v p = GL.glProgramUniform3#{postfix}v (coerce p) . coerce
    programUniform4v p = GL.glProgramUniform4#{postfix}v (coerce p) . coerce
    getUniformv p = GL.glGetUniform#{postfix}v (coerce p) . coerce
    getnUniformv p = GL.glGetnUniform#{postfix}v (coerce p) . coerce
|]

genUniformInstancePrim :: T.Text -> LT.Text
genUniformInstancePrim type' =
    [lt|instance Uniform #{type'} where
    uniform loc a0 = uniform1 (coerce loc) a0
    uniformv loc vec = liftIO . Vector.unsafeWith vec $
        uniform1v (coerce loc) (fromIntegral . Vector.length $ vec)
    programUniform program loc a0 = programUniform1 program (coerce loc) a0
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $
        programUniform1v program (coerce loc) (fromIntegral . Vector.length $ vec)
|]

genUniformInstanceVec :: Int -> LT.Text
genUniformInstanceVec len =
    [lt|instance {-# OVERLAPS #-} UniformComponent a => Uniform (V#{len} a) where
    uniform loc (V#{len} #{vargs}) = uniform#{len} (coerce loc) #{vargs}
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        uniform#{len}v (coerce loc) (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)
    programUniform program loc (V#{len} #{vargs}) = programUniform#{len} program (coerce loc) #{vargs}
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        programUniform#{len}v program (coerce loc) (fromIntegral . Vector.length $ vec) (coerce p :: Ptr a)
|]
    where
    vargs = LT.intercalate " " ["a" <> LT.pack (show x) |x <- [0..(len - 1)]]

genUniformInstanceMat :: Int -> Int -> T.Text -> T.Text -> LT.Text
genUniformInstanceMat row col type' postfix =
    [lt|instance Uniform (V#{row} (V#{col} #{type'})) where
    uniform loc a = liftIO . Foreign.with a $ \p ->
        #{ufunc}v (coerce loc) 1 GL.GL_TRUE (coerce p)
    uniformv loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        #{ufunc}v (coerce loc) (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
    programUniform program loc a = liftIO . Foreign.with a $ \p ->
        #{pufunc}v (coerce program) (coerce loc) 1 GL.GL_TRUE (coerce p)
    programUniformv program loc vec = liftIO . Vector.unsafeWith vec $ \p ->
        #{pufunc}v (coerce program) (coerce loc) (fromIntegral . Vector.length $ vec) GL.GL_TRUE (coerce p)
|]
    where
    ufunc = genUniformMatrixFunctionName "GL.glUniformMatrix" row col postfix
    pufunc = genUniformMatrixFunctionName "GL.glProgramUniformMatrix" row col postfix

genUniformMatrixFunctionName :: T.Text -> Int -> Int -> T.Text -> LT.Text
genUniformMatrixFunctionName base row col post
    | row == col = [lt|#{base}#{col}#{post}|]
    | otherwise = [lt|#{base}#{col}x#{row}#{post}|]
