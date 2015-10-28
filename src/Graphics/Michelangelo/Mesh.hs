-- |
-- Module      : Southpaw.Michelangelo.Mesh
-- Description : descr
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created July 29 2015

-- TODO | - Index buffers
--        - Use lenses (?)
--        - Move shader-specific stuff to Shader module

-- SPEC | -
--        -



module Southpaw.Michelangelo.Mesh where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Linear.Matrix -- 
import Linear.V3     -- 

import qualified Data.Map.Strict as Map -- 

import Control.Monad (liftM2, liftM) -- 
import Control.Applicative ((<*>))   --

import Graphics.Rendering.OpenGL (($=))                                  -- 
import qualified Graphics.Rendering.OpenGL            as GL              -- 
import qualified Graphics.Rendering.OpenGL.GL.Shaders as GLS             -- 
import qualified Graphics.Rendering.OpenGL.GL.Shaders.Uniform as Uniform --
import Graphics.GLUtil.JuicyTextures                                     -- 

import qualified Graphics.GLUtil as GLUtil -- 

import Southpaw.Michelangelo.Shaders as Shade -- 
import Southpaw.WaveFront.Load       as WFL   -- 
import Southpaw.WaveFront.Parsers    as WF    -- 



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
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



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
-- renderMesh :: Mesh -> IO ()
renderMesh :: Mesh -> IO ()
renderMesh mesh = do
	GLUtil.printErrorMsg "Entering renderMesh"
	-- return $ (prepare mesh) <*> Just mesh
	case prepare mesh of
		Just action -> action mesh
		_           -> return ()
	GLUtil.printErrorMsg "Shader program set"

	withAttributes mesh $ \ _ -> do
		GLUtil.printErrorMsg "Entering withAttributes action"
		Shade.setShaderUniforms (shader mesh) (Map.elems $ uniforms mesh) -- 
		GLUtil.printErrorMsg "Uniforms set"
		GL.drawArrays (primitive mesh) 0 (fromIntegral $ size mesh)       -- 
		GLUtil.printErrorMsg "Arrays drawn"


-- | 
-- uniform :: GL.UniformComponent u => GL.UniformLocation -> u -> IO () 
-- uniform loc u = GL.uniform loc u


-- |
-- TODO: Are there any attributes that are NOT buffers (?)
bufferAttribute :: GL.BufferObject -> GL.AttribLocation -> Int -> IO () 
bufferAttribute buffer loc count = do
	GL.vertexAttribArray loc     $= GL.Enabled                                                                            -- 
	GL.bindBuffer GL.ArrayBuffer $= Just buffer                                                                           -- 
	GL.vertexAttribPointer loc   $= (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral count) GL.Float 0 GLUtil.offset0) -- 


-- |
attribute :: GL.Program -> String -> GL.BufferObject -> Int -> IO ()
attribute program name buffer count = do
	loc <- GL.get $ GL.attribLocation program name
	bufferAttribute buffer loc count


-- |
bindAttributes :: Mesh -> IO ()
bindAttributes mesh = do
	-- GLS.currentProgram $= Just (shader mesh)
	Map.foldrWithKey reduce nothing (attributes mesh) -- TODO: Use Traversable instead (?)
	where
	   reduce key (loc, buff, count) acc = acc >> bufferAttribute buff loc count -- 
	   nothing                           = return ()                             -- 


-- |
unbindAttributes :: Mesh -> IO ()
unbindAttributes mesh = Map.foldrWithKey reduce nothing (attributes mesh)
	where
	   reduce key (loc, buff, count) acc = acc >> (GL.vertexAttribArray loc $= GL.Enabled) -- 
	   nothing                           = return ()                                       -- 


-- | 
withAttributes :: Mesh -> (Mesh -> IO ()) -> IO ()
withAttributes mesh action = do
	bindAttributes mesh
	action mesh
	unbindAttributes mesh


-- | 
-- withUniforms :: Mesh -> IO ()


-- |
{-
prepareTextured :: state -> IO ()
prepareTextured _ = do
	maybe
	  (return ())
	  (\(tex, coords) -> do
	  	GL.bindBuffer GL.ArrayBuffer $= Just coords
	  	GL.vertexAttribPointer (texattrib) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 GLUtil.offset0))
	  (liftM2 (,) (texture mesh) (texcoords mesh))
-}