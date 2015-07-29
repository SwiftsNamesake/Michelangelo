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



module Southpaw.Michelangelo.Shaders where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
import Graphics.GLUtil hiding (loadShaderProgram)

import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

import Linear.Matrix
import Linear.Projection
import Linear.Quaternion
import Linear.V3

import qualified Foreign.Marshal.Utils as Marshal (with)
import Foreign.Ptr (castPtr, Ptr())

import Control.Lens

import Control.Exception

import Text.Printf



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
data ShaderProgram = ShaderProgram {}



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Improve control flow
-- TODO: Improve error checking (eg. which logs belong to which part, check errors at each stage?)
-- TODO: Catch exceptions
-- TODO: Program crashes when the source strings are empty
-- TODO: Optional logging layer (?)
createShaderProgram :: String -> String -> IO (Either [String] GL.Program)
createShaderProgram vsource psource = do
	putStrLn "Creating shader program"
	program <- GL.createProgram
	vshader <- GL.createShader VertexShader
	pshader <- GL.createShader FragmentShader
	printError

	putStrLn "Setting vertex shader source"
	shaderSourceBS vshader $= packUtf8 vsource

	putStrLn "Compiling vertex shader"
	compileShader vshader

	putStrLn "Setting fragment shader source"
	shaderSourceBS pshader $= packUtf8 psource
	compileShader pshader

	-- putStrLn "Compiling shaders..."

	vstatus <- GL.get $ compileStatus vshader
	print vstatus
	pstatus <- GL.get $ compileStatus pshader
	print pstatus

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
setShaderUniforms :: GL.Program -> V3 Float -> V3 Float -> IO ()
setShaderUniforms program t (V3 rx ry rz) = do
	-- activeUniforms program >>= print
	-- (let (V3 x y z) = t in printf "(%.02f, %.02f, %.02f)\n" x y z)
	GL.UniformLocation uMVMatrix <- GL.uniformLocation program "uMVMatrix" -- uniform mat4 uMVMatrix;
	GL.UniformLocation uPMatrix  <- GL.uniformLocation program "uPMatrix"  -- uniform mat4 uPMatrix;
	-- print (uMVMatrix, uPMatrix)
	-- let modelview = (identity !*! fromQuaternion (Quaternion 1.0 r)) & (translation .~ t)

	let rotation  = transpose . m33_to_m44 . fromQuaternion $ axisAngle (V3 0 1 0) (ry)
	let modelview = transpose $ (identity) & (translation .~ t)

	-- TODO: Experiment with inverse perspective
	-- let projection = perspective 40.0 1.0 1.0 10.0
	let projection = perspective
	                   (radians 40.0) -- FOV (y direction, in radians)
	                   1.0            -- Aspect ratio
	                   1.0            -- Near plane
	                   20.0           -- Far plane

	-- print modelview
	Marshal.with modelview  $ \ptr -> GLRaw.glUniformMatrix4fv uMVMatrix 1 0 (castPtr (ptr :: Ptr (M44 Float)))
	Marshal.with projection $ \ptr -> GLRaw.glUniformMatrix4fv uPMatrix  1 0 (castPtr (ptr :: Ptr (M44 Float)))
	-- uniform uPMatrix  $= _
	where
	  radians deg = deg * pi/180.0
	  degrees rad = rad * 180.0/pi


-- |
loadShaderProgram :: String -> String -> IO (Either [String] GL.Program)
loadShaderProgram vpath ppath = do
	vsource <- readFile vpath
	psource <- readFile ppath

	catch
	  (createShaderProgram vsource psource)            --
	  caught -- TODO: More elaborate exception message (?)
	where
	  caught :: IOException -> IO (Either [String] GL.Program)
	  caught e = return $ Left ["Unable to open file."]
