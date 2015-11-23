-- |
-- Module      : Graphics.Michelangelo.Mesh
-- Description :
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
--        - Proper logging

-- SPEC | -
--        -



module Graphics.Michelangelo.Mesh where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Linear.Matrix --
import Linear.V3     --

import qualified Data.Map.Strict as Map --

import Control.Lens
import Control.Monad (liftM2, liftM) --
import Control.Applicative ((<*>))   --

import           Graphics.Rendering.OpenGL (($=))                        --
import qualified Graphics.Rendering.OpenGL            as GL              --
import qualified Graphics.Rendering.OpenGL.GL.Shaders as GLS             --
import qualified Graphics.Rendering.OpenGL.GL.Shaders.Uniform as Uniform --
import           Graphics.GLUtil.JuicyTextures                           -- 

import qualified Graphics.GLUtil as GLUtil --

import Graphics.WaveFront.Load       as WFL --
import Graphics.WaveFront.Parsers    as WF  --

import Graphics.Michelangelo.Types
import Graphics.Michelangelo.Lenses
import Graphics.Michelangelo.Shaders as Shade --



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- renderMesh :: Mesh -> IO ()
renderMesh :: Mesh -> IO ()
renderMesh mesh = do
  GLUtil.printErrorMsg "Entering renderMesh"
  -- return $ (prepare mesh) <*> Just mesh
  maybe (return ()) ($ mesh) (mesh^.prepare)
  GLUtil.printErrorMsg "Shader program set"

  withAttributes mesh $ \ _ -> do
    GLUtil.printErrorMsg "Entering withAttributes action"                   --
    Shade.setShaderUniforms (mesh^.shader) (Map.elems $ _meshUniforms mesh) --
    GLUtil.printErrorMsg "Uniforms set"                                     --
    GL.drawArrays (mesh^.primitive) 0 (fromIntegral $ mesh^.size)           --
    GLUtil.printErrorMsg "The arrays have been rendered, my liege"          --


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
  Map.foldrWithKey reduce nothing (mesh^.attributes) -- TODO: Use Traversable instead (?)
  where
     reduce key (loc, buff, count) acc = acc >> bufferAttribute buff loc count --
     nothing                           = return ()                             --


-- |
unbindAttributes :: Mesh -> IO ()
unbindAttributes mesh = Map.foldrWithKey reduce nothing (mesh^.attributes)
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
