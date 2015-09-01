-- |
-- Module      : Southpaw.Michelangelo.Shaders
-- Description : OpenGL shader utilities
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created July 27 2015

-- TODO | - 
--        - 

-- SPEC | -
--        -



---------------------------------------------------------------------------------------------------
-- Pragmas
---------------------------------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-} --
{-# LANGUAGE FlexibleInstances    #-} --



---------------------------------------------------------------------------------------------------
-- API
---------------------------------------------------------------------------------------------------
module Southpaw.Michelangelo.Shaders where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

-- import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
import Graphics.Rendering.OpenGL.GL.Shaders
import Graphics.GLUtil hiding (loadShaderProgram)

import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

import Linear.Matrix
import Linear.Projection
import Linear.Quaternion
import Linear.V3
import Linear.V4

import Foreign.Storable (Storable)
import qualified Foreign.Marshal.Utils as Marshal (with)
import Foreign.Ptr (castPtr, Ptr())

import qualified Data.Map as Map

import Control.Lens
import Control.Exception
import Control.Monad (forM)

import Text.Printf



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- |
data ShaderProgram = ShaderProgram { _program    :: GL.Program,
	                                 _attributes :: Map.Map String (GL.AttribLocation,  GL.VariableType),
                                     _uniforms   :: Map.Map String (GL.UniformLocation, GL.VariableType) }


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



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Improve control flow
-- TODO: Improve error checking (eg. which logs belong to which part, check errors at each stage?)
-- TODO: Catch exceptions
-- TODO: Program crashes when the source strings are empty
-- TODO: Optional logging layer (?)
-- TODO: Use Monad transformer to make 'bailing-out' easier (?)
createShaderProgram :: String -> String -> IO (Either [String] GL.Program)
createShaderProgram vsource psource = do
	putStrLn "Creating shader program"
	program <- GL.createProgram
	vshader <- GL.createShader VertexShader
	pshader <- GL.createShader FragmentShader

	case (vsource, psource) of
		("", _) -> return $ Left ["Empty vertex shader source"]
		(_, "") -> return $ Left ["Empty pixel shader source"]
		_       -> do
			putStrLn "Setting vertex shader source"
			shaderSourceBS vshader $= packUtf8 vsource

			putStrLn "Compiling vertex shader"
			compileShader vshader

			putStrLn "Setting fragment shader source"
			shaderSourceBS pshader $= packUtf8 psource
			compileShader pshader

			-- putStrLn "Compiling shaders..."

			vstatus <- GL.get $ compileStatus vshader
			printf "Vertex shader %s compiled successfully.\n" (if vstatus then "was" else "was not")
			pstatus <- GL.get $ compileStatus pshader
			printf "Vertex pixel %s compiled successfully.\n" (if pstatus then "was" else "was not")

			if vstatus && pstatus
				then do
					putStrLn "Successfully compiled shaders. Linking program..."
					mapM (GL.attachShader program) [vshader, pshader]

					GL.linkProgram program
					linked <- GL.get $ GL.linkStatus program
					if linked
						then return $ Right program
						else mapM GL.get [GL.shaderInfoLog vshader, GL.shaderInfoLog pshader, GL.programInfoLog program] >>= return . Left
				else mapM (GL.get . GL.shaderInfoLog) [vshader, pshader] >>= return . Left


-- |
-- TODO: Rename (?)
-- TODO: Pass in uniforms by name or by location (?)
setShaderUniforms :: GL.Program -> [(GL.UniformLocation, UniformValue)] -> IO ()
setShaderUniforms theprogram theuniforms = do
	-- Set uniforms
	-- mapM ((>> printErrorMsg "Setting uniform") . uncurry uniform) theuniforms
	-- TODO: Refactor
	forM theuniforms $ \(loc, value) -> case value of
		UMatrix44 mat -> uniform loc $= mat
		UFloat    f   -> uniform loc $= f
		UInt      i   -> uniform loc $= i
		-- UVec vec -> uniform loc $= vec
	return ()


-- |
loadShaderProgram :: String -> String -> IO (Either [String] GL.Program)
loadShaderProgram vpath ppath = do
	[vsource, psource] <- mapM readFile [vpath, ppath]
	catch
	  (createShaderProgram vsource psource)            --
	  caught -- TODO: More elaborate exception message (?)
	where
	  caught :: IOException -> IO (Either [String] GL.Program)
	  caught _ = return $ Left ["Unable to open file."]



---------------------------------------------------------------------------------------------------
-- Uniforms
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Better naming conventions
-- TOOD: All uniform types (matrices, vectors, scalars, variable length)
data UniformValue = UMatrix44 (M44 Float) |
                    UVec3     (V3 Float)  |
                    UFloat     Float      |
                    UInt       Int
                    deriving (Show)


-- |
-- uniform :: GL.UniformLocation -> UniformValue -> IO ()
-- uniform (GL.UniformLocation loc) (UMatrix44 mat) = Marshal.with mat $ \ptr -> GLRaw.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 Float)))
-- uniform (GL.UniformLocation loc) (UVec3     vec) = Marshal.with vec $ \ptr -> GLRaw.glUniform3fv       loc 1   (castPtr (ptr :: Ptr (V3 Float)))
-- uniform (GL.UniformLocation loc) (UFloat    f)   = Marshal.with f   $ \ptr -> GLRaw.glUniform1fv       loc 1   (castPtr (ptr :: Ptr (Float)))
-- uniform (GL.UniformLocation loc) (UInt      i)   = Marshal.with i   $ \ptr -> GLRaw.glUniform1iv       loc 1   (castPtr (ptr :: Ptr (Int)))


-- TODO: Move instances to Instances or Uniform module (?)
-- TODO: Transpose via OpenGL or Linear (?)

-- |
-- TODO: Better names (?)
-- class UniformValue u where
	-- setUniform :: (Storable u) => GL.GLint -> GL.GLsizei -> Ptr GL.GLfloat -> IO ()


-- Scalars


-- Vectors
-- instance UniformValue (M44 Float) where
	-- setUniform = GLRaw.glUniformMatrix3fv


-- Matrices
-- instance UniformValue (M22 Float) where
	-- setUniform = GLRaw.glUniformMatrix2fv


-- instance UniformValue (M33 Float) where
	-- setUniform = GLRaw.glUniformMatrix3fv


-- instance UniformValue (M44 Float) where
	-- setUniform = GLRaw.glUniformMatrix4fv

-- glUniform1f :: GLint -> GLfloat -> IO ()
-- glUniform2f :: GLint -> GLfloat -> GLfloat -> IO ()
-- glUniform3f :: GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()
-- glUniform4f :: GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- glUniform1i :: GLint -> GLint -> IO ()
-- glUniform2i :: GLint -> GLint -> GLint -> IO ()
-- glUniform3i :: GLint -> GLint -> GLint -> GLint -> IO ()
-- glUniform4i :: GLint -> GLint -> GLint -> GLint -> GLint -> IO ()

-- glUniform1fv :: GLint -> GLsizei -> Ptr GLfloat -> IO () --
-- glUniform2fv :: GLint -> GLsizei -> Ptr GLfloat -> IO () --
-- glUniform3fv :: GLint -> GLsizei -> Ptr GLfloat -> IO () --
-- glUniform4fv :: GLint -> GLsizei -> Ptr GLfloat -> IO () --
-- glUniform1iv :: GLint -> GLsizei -> Ptr GLint   -> IO () --
-- glUniform2iv :: GLint -> GLsizei -> Ptr GLint   -> IO () --
-- glUniform3iv :: GLint -> GLsizei -> Ptr GLint   -> IO () --
-- glUniform4iv :: GLint -> GLsizei -> Ptr GLint   -> IO () --

-- glUniformMatrix2fv :: GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO () -- (✓)
-- glUniformMatrix3fv :: GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO () -- (✓)
-- glUniformMatrix4fv :: GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO () -- (✓)