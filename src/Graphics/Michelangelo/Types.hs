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




--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Graphics.Michelangelo.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------




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
data Mesh = Mesh { texture    :: Maybe GL.TextureObject,   -- TODO: Allow more than one texture (?)
                   primitive  :: GL.PrimitiveMode,         --
                   attributes :: Map.Map String Attribute, --
                   uniforms   :: Map.Map String (GL.UniformLocation, UniformValue),   --
                   shader     :: GL.Program,               --
                   prepare    :: Maybe (Mesh -> IO ()),    -- Optional rendering setup function
                   centre     :: V3 Float,                 --
                   bounds     :: WF.BoundingBox Float,     --
                   size       :: Int                       --
                 } --deriving (Show)


-- |
-- data Scene = Scene { _camera :: Camera, _entities :: [Entity],  }


-- |
data ShaderProgram = ShaderProgram { _program    :: GL.Program,
	                                   _attributes :: Map.Map String (GL.AttribLocation,  GL.VariableType),
                                     _uniforms   :: Map.Map String (GL.UniformLocation, GL.VariableType)}


-- |
-- type Uniform   = (GL.UniformLocation, UniformValue)         --
type Attribute = (GL.AttribLocation,  GL.BufferObject, Int) -- TODO: Are there any non-buffer attribute types, separate type (?)


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
