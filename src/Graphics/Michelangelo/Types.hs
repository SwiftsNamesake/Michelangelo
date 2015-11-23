-- |
-- Module      : Graphics.Michelangelo.Types
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 30 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-} --
{-# LANGUAGE FlexibleInstances    #-} --



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Graphics.Michelangelo.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.Map as Map
import qualified Graphics.WaveFront.Parsers as WF

import           Foreign.Storable                 (Storable)
import           Foreign.Ptr                      (castPtr, Ptr())
import qualified Foreign.Marshal.Utils as Marshal (with)

import Linear.V3
import Linear.Matrix

import qualified Graphics.Rendering.OpenGL            as GL              --
import           Graphics.Rendering.OpenGL            as GL              --
import qualified Graphics.Rendering.OpenGL.GL.Shaders as GLS             --
import qualified Graphics.Rendering.OpenGL.Raw        as GLRaw           --
import qualified Graphics.Rendering.OpenGL.GL.Shaders.Uniform as Uniform --
import           Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import           Graphics.GLUtil



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
data Entity = Entity {}


-- |
-- TODO: Include shader program, attributes, uniforms, etc. (?)
-- TODO: Store render function (?)
-- TODO: Restructure (?)
-- TODO: Metadata (?)
-- TODO: Less ugly field names (maybe tweak the name mangler?)
data Mesh = Mesh { _meshTexture    :: Maybe GL.TextureObject,   -- TODO: Allow more than one texture (?)
                   _meshPrimitive  :: GL.PrimitiveMode,         --
                   _meshAttributes :: Map.Map String Attribute, --
                   _meshUniforms   :: Map.Map String (GL.UniformLocation, UniformValue),   --
                   _meshShader     :: GL.Program,               --
                   _meshPrepare    :: Maybe (Mesh -> IO ()),    -- Optional rendering setup function
                   _meshCentre     :: V3 Float,                 --
                   _meshBounds     :: WF.BoundingBox Float,     --
                   _meshSize       :: Int                       --
                 } --deriving (Show)


-- |
-- data Scene = Scene { _camera :: Camera, _entities :: [Entity],  }


-- |
-- TODO: Rename (?)
-- TODO: Less ugly field names (maybe tweak the name mangler?)
data ShaderProgram = ShaderProgram { _shaderProgramProgram    :: GL.Program,
                                     _shaderProgramAttributes :: Map.Map String (GL.AttribLocation,  GL.VariableType),
                                     _shaderProgramUniforms   :: Map.Map String (GL.UniformLocation, GL.VariableType)}


-- |
-- type Uniform   = (GL.UniformLocation, UniformValue)         --
type Attribute = (GL.AttribLocation, GL.BufferObject, Int) -- TODO: Are there any non-buffer attribute types, separate type (?)

-- Classes ---------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Better naming conventions
-- TODO: Use type family (?)
-- TOOD: All uniform types (matrices, vectors, scalars, variable length)
data UniformValue = UMatrix44 (M44 Float) |
                    UVec3     (V3 Float)  |
                    UFloat     Float      |
                    UInt       Int
                    deriving (Show)

-- Instances -------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Make polymorphic (?)
-- TODO: Refactor
-- TODO: Implement uniformv
-- TODO: Find out how to read uniform value
-- instance GL.UniformComponent a => GL.Uniform (M44 a) where
instance GL.Uniform (M44 Float) where
  uniform (GL.UniformLocation loc) = GL.makeStateVar (error "Not implemented") (\u -> Marshal.with (transpose u) (\ptr -> GLRaw.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 Float)))))
   -- uniformv loc count = uniform3v location count . (castPtr :: Ptr (Vertex3 b) -> Ptr b)


instance GL.Uniform (Float) where
  uniform (GL.UniformLocation loc) = GL.makeStateVar (error "Not implemented") (\f -> Marshal.with f (\ptr -> GLRaw.glUniform1fv loc 1 (castPtr (ptr :: Ptr (Float)))))
   -- uniformv loc count = uniform3v location count . (castPtr :: Ptr (Vertex3 b) -> Ptr b)


-- |
instance GL.Uniform (Int) where
  uniform (GL.UniformLocation loc) = GL.makeStateVar (error "Not implemented") (\i -> Marshal.with i (\ptr -> GLRaw.glUniform1iv loc 1 (castPtr (ptr :: Ptr (Int)))))
   -- uniformv loc count = uniform3v location count . (castPtr :: Ptr (Vertex3 b) -> Ptr b)
